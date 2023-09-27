-- |
-- Module      : Parser
-- Description : Lily picker parser
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Parser (
    parseLily,
) where

import Control.Monad (forM_, void, when)
import Control.Monad.State.Strict
import Data.DList qualified as DL
import Data.Foldable (find, foldl')
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (State)

import Types

-- | Lily picker parser state
data ParserState = ParserState
    { -- | List of currently active parts
      activePartNames :: [PartName]
    }
    deriving stock (Eq, Show)

initialParserState :: ParserState
initialParserState =
    ParserState
        { activePartNames = []
        }

-- | Type for the parser
type Parser a = StateT ParserState (Parsec Void T.Text) a

-- | Parse a Lily picker file.
-- The string argument is the name of the source.
parseLily :: String -> T.Text -> Either T.Text Lily
parseLily fp text = case parse (evalStateT topLevel initialParserState) fp text of
    Left err -> Left . T.pack . errorBundlePretty $! err
    Right r -> Right r

-- | A parser qhich returns the result of a text parser together with the
-- source location of the parse's starting point.
located :: Parser T.Text -> Parser LocatedText
located p = do
    pos <- getSourcePos
    text <- p
    pure $! (LocatedText {pos = pos, value = text})

-- | Top-level parser
topLevel :: Parser Lily
topLevel = lilyFile emptyLily

lilyFile :: Lily -> Parser Lily
lilyFile lily = lilyLines lily

lilyLines :: Lily -> Parser Lily
lilyLines lily = (lilyLine lily >>= lilyLines) <|> (eof *> (pure $! lily))

lilyLine :: Lily -> Parser Lily
lilyLine lily =
    ( label "part definition" (partDefs lily)
        <|> label "part extension" (partExtension lily)
        <|> label "parallel music" (parMusic lily)
        <|> label "shared music" (sharedMusic lily)
        <|> label "invidual parts music" (individualMusic lily)
        <|> label "preamble" (appendPreamble lily)
        <|> label "epilogue" (appendEpilogue lily)
        <|> label "comment" (comment *> (pure $! lily))
        <|> (pure $! lily)
    )
        <* single '\n'

partDefs :: Lily -> Parser Lily
partDefs lily = do
    single '=' *> space
    defs <- sepEndBy1 partDef (single '|' *> space)
    let partNames = fmap (.name.value) defs
    forM_ partNames \name -> do
        when (HM.member name lily.parts) $
            fail ("Attempt to redefine a part: " <> show name)
    modify' (\s -> s {activePartNames = partNames})
    pure
        lily
            { parts = foldl' addPart lily.parts defs
            }
  where
    addPart :: PartMap -> Part -> PartMap
    addPart partMap part =
        HM.insert part.name.value part partMap

partDef :: Parser Part
partDef = do
    name <- located varName
    function <- optional $! try $! space1 *> textTillBar1
    void $! space
    pure $!
        ( Part
            { name = name {pos = name.pos {sourceColumn = pos1}},
              function = function,
              contents = mempty
            }
        )

partExtension :: Lily -> Parser Lily
partExtension lily = do
    single '>' *> space
    partNames <- sepEndBy1 varName (space *> single '|' *> space)
    forM_ partNames \name -> do
        when (not $! HM.member name lily.parts) $
            fail ("Attempt to extend an undefined part: " <> show name)
    modify' \s -> s {activePartNames = partNames}
    pure $! lily

parMusic :: Lily -> Parser Lily
parMusic lily = do
    void $! single '|'
    preBar <- single '|' *> pure True <|> pure False

    mps <- sepEndBy1 (located textTillBar1) (single '|')
    postBar <- single '|' *> pure True <|> pure False
    void $! space

    partNames <- gets (.activePartNames)
    let firstNonBlank = find (not . isBlank) $! fmap (.value) mps
        skipMusic = maybe "" (\m -> "\\skip {" <> m <> "}") firstNonBlank
    let !mps' = fmap (processMusic preBar postBar skipMusic) mps
    partMap <- addMusic partNames mps' lily.parts
    pure lily {parts = partMap}
  where
    addMusic :: [PartName] -> [LocatedText] -> PartMap -> Parser PartMap
    addMusic [] [] partMap = pure $! partMap
    addMusic [] ms _ = fail $! "Too many music expressions in parallel music: " <> show (fmap (.value) ms)
    addMusic ps [] _ = fail $! "Missing expressions in parallel music: " <> show ps
    addMusic (partName : partNames) (music : musics) partMap = do
        addMusic partNames musics $! addToPart partName music partMap

    isBlank :: T.Text -> Bool
    isBlank = T.all (== ' ')

    processMusic :: Bool -> Bool -> T.Text -> LocatedText -> LocatedText
    processMusic preBar postBar skip (LocatedText {pos, value = music}) =
        let pos' =
                if preBar
                    then pos {sourceColumn = mkPos $! (unPos pos.sourceColumn) - 1}
                    else pos
            music' =
                (if preBar then "|" else "")
                    <> (if isBlank music then skip else music)
                    <> (if postBar then " |" else "")
        in  LocatedText {pos = pos', value = music'}

addToPart :: PartName -> LocatedText -> PartMap -> PartMap
addToPart partName music partMap =
    if not $! HM.member partName partMap
        then error ("Key " <> show partName <> " missing from part mapping " <> show partMap)
        else HM.adjust (\part -> part {contents = part.contents `DL.snoc` music}) partName partMap

sharedMusic :: Lily -> Parser Lily
sharedMusic lily = do
    void $! single '*'
    partNames <- gets (.activePartNames)
    when (null partNames) $
        fail "Shared music without any active parts"
    music <- located textLine
    let partMap = foldl' (\acc name -> addToPart name music acc) lily.parts partNames
    pure $! (lily {parts = partMap})

individualMusic :: Lily -> Parser Lily
individualMusic lily = do
    partNames <- someTill (varName <* space) (single ':')
    music <- located textLine
    let partMap = foldl' (\acc name -> addToPart name music acc) lily.parts partNames
    pure $! (lily {parts = partMap})

appendPreamble :: Lily -> Parser Lily
appendPreamble lily = do
    void $! single '-'
    text <- located textLine
    pure lily {preamble = lily.preamble `DL.snoc` text}

appendEpilogue :: Lily -> Parser Lily
appendEpilogue lily = do
    void $! single '+'
    text <- located textLine
    pure lily {epilogue = lily.epilogue `DL.snoc` text}

comment :: Parser ()
comment = single '%' *> takeWhile1P Nothing (\ch -> ch /= '\n') *> pure ()

varName :: Parser T.Text
varName =
    label "variable name" $!
        textBy some \ch -> (not . isSpace) ch && isNonBarText ch && ch /= ':'

textTillBar1 :: Parser T.Text
textTillBar1 = textBy some isNonBarText

textBy :: (Parser T.Text -> Parser [T.Text]) -> (Char -> Bool) -> Parser T.Text
textBy repeater predicate =
    fmap (T.dropWhileEnd isSpace . T.concat) $!
        repeater $!
            (escape <?> "escape sequence")
                <|> takeWhile1P (Just "text") \ch -> ch /= '$' && predicate ch
  where
    escape :: Parser T.Text
    escape = do
        void $! single '$'
        fmap T.singleton . oneOf $! ['$', ':', '|']

textLine :: Parser T.Text
textLine = textBy many isText

isText :: Char -> Bool
isText ch = ch >= ' ' || ch == '\t'

isNonBarText :: Char -> Bool
isNonBarText ch = isText ch && ch /= '|'

space :: Parser ()
space = void (takeWhileP (Just "space") isSpace)

space1 :: Parser ()
space1 = void (takeWhile1P (Just "space") isSpace)

isSpace :: Char -> Bool
isSpace ch = ch == ' ' || ch == '\t'
