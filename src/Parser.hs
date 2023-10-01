-- |
-- Module      : Parser
-- Description : Lily picker parser
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Parser (
    parseLilyFile,
    parseLilyText,
) where

import Control.Monad (foldM, forM_, void, when)
import Control.Monad.State.Strict
import Data.DList qualified as DL
import Data.Foldable (find, foldl')
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Void (Void)
import System.File.OsPath
import System.OsPath
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
type Parser a = StateT ParserState (ParsecT Void T.Text IO) a

-- | Parse Lily picker input from a Text value. The OsPath argument is the
-- name of the input source, and is only used for displaying parse errors
-- (both in Lily picker and LilyPond syntax).
parseLilyText :: OsString -> T.Text -> IO (Either T.Text Lily)
parseLilyText fp input = do
    runLilyParser topLevel initialParserState fp input >>= \case
        Right (r, _) -> pure . Right $! r
        Left err -> pure . Left . T.pack . errorBundlePretty $! err

-- | Parse Lily picker input from a file.
parseLilyFile :: OsPath -> IO (Either T.Text Lily)
parseLilyFile fp = do
    input <- decodeUtf8Lenient <$> readFile' fp
    parseLilyText fp input

-- | Run the Lily picker parser
runLilyParser
    :: Parser a -> ParserState -> OsPath -> T.Text -> IO (Either (ParseErrorBundle T.Text Void) (a, ParserState))
runLilyParser p s fp input = do
    fp' <- decodeFS fp
    runParserT (runStateT p s) fp' input

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
    ( partDefs lily
        <|> partExtension lily
        <|> parMusic lily
        <|> sharedMusic lily
        <|> appendPreamble lily
        <|> appendEpilogue lily
        <|> (comment *> (pure $! lily))
        <|> individualMusic lily
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
        partMap' <- addToPart partName music partMap
        addMusic partNames musics partMap'

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

addToPart :: PartName -> LocatedText -> PartMap -> Parser PartMap
addToPart partName music partMap = do
    when (not $! HM.member partName partMap) $
        fail
            ("Attempt to add music to undefined part: " <> show partName)
    pure $!
        HM.adjust (\part -> part {contents = part.contents `DL.snoc` music}) partName partMap

sharedMusic :: Lily -> Parser Lily
sharedMusic lily = do
    void $! single '*'
    partNames <- gets (.activePartNames)
    when (null partNames) $
        fail "Shared music without any active parts"
    music <- located textLine
    partMap <- foldM (\acc name -> addToPart name music acc) lily.parts partNames
    pure $! (lily {parts = partMap})

individualMusic :: Lily -> Parser Lily
individualMusic lily = do
    partNames <- someTill (varName <* space) (single ':')
    music <- located textLine
    partMap <- foldM (\acc name -> addToPart name music acc) lily.parts partNames
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
varName = word <?> "variable name"

word :: Parser T.Text
word = label "word" $! textBy some $! escapeSequence <|> quotedString <|> rawText isWordChar

textTillBar1 :: Parser T.Text
textTillBar1 = label "text |" $! textBy some (escapeSequence <|> rawText isNonBarText)

textLine :: Parser T.Text
textLine = label "text line" $! textBy many (escapeSequence <|> rawText isText)

-- | `textBy` parses a string from the input file using the specified repater
-- parser (i.e. `some` or `many` and a string parser, such as a combination of
-- `quotedString`, `rawText` and `escapeSequence`
textBy :: (Parser T.Text -> Parser [T.Text]) -> Parser T.Text -> Parser T.Text
textBy repeater p =
    T.concat <$> repeater p

rawText :: (Char -> Bool) -> Parser T.Text
rawText = takeWhile1P (Just "raw text")

escapeSequence :: Parser T.Text
escapeSequence = label "escape sequence" do
    void $! single escapeChar
    fmap T.singleton . oneOf $! escapableChars

escapeChar :: Char
escapeChar = '$'

escapableChars :: [Char]
escapableChars =
    [ ' ',
      '"',
      '#',
      '$',
      '%',
      '(',
      ')',
      '*',
      '+',
      '-',
      ':',
      '<',
      '=',
      '>',
      '[',
      ']',
      '{',
      '|',
      '}'
    ]

quotedString :: Parser T.Text
quotedString = label "quoted string" do
    void $! single '"'
    text <- textBy many (escapeSequence <|> rawText isQuotedText)
    void $! single '"'
    pure $! text

isQuotedText :: Char -> Bool
isQuotedText ch = isText ch && ch /= '"'

isText :: Char -> Bool
isText ch = ch /= escapeChar && (ch >= ' ' || ch == '\t')

isNonBarText :: Char -> Bool
isNonBarText ch = isText ch && ch /= '|'

isWordChar :: Char -> Bool
isWordChar ch = isText ch && not (isSpace ch) && ch `notElem` escapableChars

space :: Parser ()
space = void (takeWhileP (Just "space") isSpace)

space1 :: Parser ()
space1 = void (takeWhile1P (Just "space") isSpace)

isSpace :: Char -> Bool
isSpace ch = ch == ' ' || ch == '\t'
