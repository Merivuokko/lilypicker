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

import Control.Monad (void, when)
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.Foldable (foldl')
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TL
import Data.Void (Void)
import Text.Megaparsec hiding (State)

import Types

-- | Lily picker parser state
data ParserState = ParserState
    { -- | List of currently defined part names
      partNames :: [PartName]
    }
    deriving stock (Eq, Show)

initialParserState :: ParserState
initialParserState =
    ParserState
        { partNames = []
        }

-- | Type for the parser
type Parser a = StateT ParserState (Parsec Void T.Text) a

-- | Parse a Lily picker file.
-- The string argument is the name of the source.
parseLily :: String -> T.Text -> Either T.Text Lily
parseLily fp text = case parse (evalStateT topLevel initialParserState) fp text of
    Left err -> Left . T.pack . errorBundlePretty $! err
    Right r -> Right r

-- | Top-level parser
topLevel :: Parser Lily
topLevel = lilyFile emptyLily

lilyFile :: Lily -> Parser Lily
lilyFile lily = space *> lilyLines lily

lilyLines :: Lily -> Parser Lily
lilyLines lily = (lilyLine lily >>= lilyLines) <|> (eof *> (pure $! lily))

lilyLine :: Lily -> Parser Lily
lilyLine lily =
    ( label "part definition" (partDefs lily)
        <|> label "parallel music" (parMusic lily)
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
    modify' (\s -> s {partNames = fmap fst defs})
    pure
        lily
            { parts = foldl' addPart lily.parts defs
            }
  where
    addPart :: PartMap -> (PartName, Maybe T.Text) -> PartMap
    addPart parts (partName, function) =
        let newPart = Part {contents = mempty, function = function}
        in  HM.insertWith (\_new old -> old) partName newPart parts

partDef :: Parser (PartName, Maybe T.Text)
partDef = do
    name <- word <?> "variable name"
    function <- optional $! try $! space1 *> textTillBar1
    void $! space
    pure $! (name, function)

parMusic :: Lily -> Parser Lily
parMusic lily = do
    void $! single '|'
    mps <- sepEndBy ((,) <$> getSourcePos <*> textTillBar1) (try $! single '|' *> notFollowedBy (single '|'))
    barLine <- (try $! chunk "||") *> pure True <|> pure False
    void $! space
    partNames <- gets (.partNames)

    let mps' = if barLine then fmap (second (<> " |")) mps else mps
    partMap <- addMusic partNames mps' lily.parts
    pure lily {parts = partMap}
  where
    addMusic :: [PartName] -> [(SourcePos, T.Text)] -> PartMap -> Parser PartMap
    addMusic [] [] partMap = pure $! partMap
    addMusic [] ms _ = fail $! "Too many music expressions in parallel music: " <> show (fmap fst ms)
    addMusic ps [] _ = fail $! "Missing expressions in parallel music: " <> show ps
    addMusic (p : ps) ((s, m) : ms) partMap = do
        when (not $! HM.member p partMap) $ error ("Key " <> show p <> " missing from part mapping " <> show partMap)
        addMusic ps ms $! HM.adjust (\part -> part {contents = part.contents <> TL.fromText (T.replicate (unPos s.sourceColumn - 1) " " <> m) <> "\n"}) p partMap

appendPreamble :: Lily -> Parser Lily
appendPreamble lily = do
    void $! single '-'
    text <- textLine
    pure lily {preamble = lily.preamble <> "\n" <> TL.fromText text}

appendEpilogue :: Lily -> Parser Lily
appendEpilogue lily = do
    void $! single '+'
    text <- textLine
    pure lily {epilogue = lily.epilogue <> "\n" <> TL.fromText text}

comment :: Parser ()
comment = single '%' *> takeWhile1P Nothing (\ch -> ch /= '\n') *> pure ()

word :: Parser T.Text
word = textBy some \ch -> (not . isSpace) ch && isNonBarText ch

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
        fmap T.singleton . oneOf $! ['$', '|']

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
