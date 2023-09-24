-- |
-- Module      : Printer
-- Description : LilyPond printer
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Printer (
    renderLily,
) where

import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha)
import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Lazy.Builder.Int qualified as TL
import Text.Megaparsec.Pos

import Types

-- | State for the printer
data PrinterState = PrinterState
    { -- | Source location of the last string printed. This is used to find out if file/line annotations are needed.
      pos :: SourcePos,
      -- | Builder of all text sent to the printer so far
      builder :: TL.Builder
    }
    deriving stock (Eq, Show)

initialPrinterState :: PrinterState
initialPrinterState =
    PrinterState
        { pos = initialPos "",
          builder = mempty
        }

-- | Printer monad
type Printer a = State PrinterState a

-- | Render a Lily data structure into pure text
renderLily :: Lily -> T.Text
renderLily lily = runPrinter (printLily lily) initialPrinterState

runPrinter :: Printer () -> PrinterState -> T.Text
runPrinter p s = (TL.toStrict . TL.toLazyText . (.builder)) $! execState p s

printLily :: Lily -> Printer ()
printLily lily = do
    printText "%%% Preamble\n"
    printDList lily.preamble
    printText "\n%%% Parts\n\n"
    mapM_ printPart $! HM.elems lily.parts
    printText "%%% Epilogue\n"
    printDList lily.epilogue

printText :: T.Text -> Printer ()
printText !text = do
    s <- get
    let !lineCount = T.count "\n" text
        !lastLineLen = T.length . T.takeWhileEnd (/= '\n') $! text
        !targetCol = mkPos $! if lineCount == 0 then unPos s.pos.sourceColumn + lastLineLen else 1 + lastLineLen
    put $!
        ( s
            { pos = s.pos {sourceLine = mkPos $! (unPos s.pos.sourceLine + lineCount), sourceColumn = targetCol},
              builder = s.builder <> TL.fromText text
            }
        )

ensureNewLine :: Printer ()
ensureNewLine = do
    col <- gets (.pos.sourceColumn)
    when (col /= pos1) $ printText "\n"

printLocation :: SourcePos -> Printer ()
printLocation newPos = do
    oldPos <- gets (.pos)
    let !sameFile = oldPos.sourceName == newPos.sourceName
        !nextLine = sameFile && oldPos.sourceLine <> pos1 == newPos.sourceLine
        !sameLine = sameFile && oldPos.sourceLine == newPos.sourceLine
        !startCol = unPos oldPos.sourceColumn
        !targetCol = unPos newPos.sourceColumn
        !showLocation = not ((sameLine && targetCol >= startCol) || nextLine)
        !newLine = (showLocation && startCol /= 1) || nextLine
        !postPadding = if newLine then targetCol - 1 else targetCol - startCol
        !locationText =
            (if newLine then "\n" else "")
                <> ( if not sameFile
                        then "\\sourcefilename \"" <> TL.fromString newPos.sourceName <> "\"\n"
                        else ""
                   )
                <> ( if not (sameLine || nextLine)
                        then "\\sourcefileline " <> (TL.decimal $! unPos newPos.sourceLine) <> "\n"
                        else ""
                   )
                <> TL.fromText (T.replicate postPadding " ")
    modify' \s ->
        s
            { pos = newPos,
              builder = s.builder <> locationText
            }

printLocatedText :: LocatedText -> Printer ()
printLocatedText (LocatedText {pos, value}) =
    printLocation pos >> printText value

printDList :: DL.DList LocatedText -> Printer ()
printDList dlist = do
    ensureNewLine
    mapM_ printLocatedText $! DL.toList dlist
    ensureNewLine

printPart :: Part -> Printer ()
printPart part = do
    ensureNewLine
    let initLine =
            LocatedText
                { pos = part.name.pos,
                  value = quoteVar part.name.value <> " =" <> maybe "" (" " <>) part.function <> " {"
                }
    printDList (DL.cons initLine part.contents)
    ensureNewLine
    printText "}\n\n"

quoteVar :: T.Text -> T.Text
quoteVar var
    | needsQuoting = quoted
    | otherwise = var
  where
    needsQuoting :: Bool
    needsQuoting = not . T.all isAlpha $! var

    quoted :: T.Text
    quoted = "\"" <> T.concatMap quoteChar var <> "\""

    quoteChar :: Char -> T.Text
    quoteChar = \case
        '"' -> "\\\""
        '\\' -> "\\\\"
        ch -> T.singleton ch
