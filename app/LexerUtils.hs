module LexerUtils where

import Numeric
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as B
import AlexTools


data State = InTop | InStringLit SourcePos B.Builder


data Token =
    TokOpenParen
  | TokCloseParen
  | TokOpenBrace
  | TokCloseBrace
  | TokOpenBracket
  | TokCloseBracket
  | TokColon
  | TokDot
  | TokDots
  | TokComma
  | TokBar
  | TokQuestion
  | TokStar
  | TokEqual
  | TokKwImport
  | TokKwType
  | TokKwAny
  | TokKwFrom
  | TokKwAs
  | TokKwStar
  | TokKwTrue
  | TokKwFalse
  | TokKwNull
  | TokKwString
  | TokKwNumber
  | TokKwBoolean
  | TokNumber
  | TokIdent
  | TokString
  | TokLineComment
  | TokError
  | TokEOF
   deriving (Show)

startStringLit :: Action State [Lexeme Token]
startStringLit =
  do 
    r <- matchRange
    setLexerState (InStringLit (sourceFrom r) mempty) 
    pure []

inStringLit ::
  (SourcePos -> B.Builder -> Action State [Lexeme Token]) ->
  Action State [Lexeme Token] 
inStringLit k =
  do
    st <- getLexerState
    case st of
      InStringLit s b -> k s b
      InTop           -> pure []

simpleStringLit :: Action State [Lexeme Token]
simpleStringLit =
  do
    s <- matchText
    r <- matchRange
    pure [ Lexeme {
      lexemeRange = r,
      lexemeToken = TokString,
      lexemeText = Text.init (Text.drop 1 s)
    } ]

addStringLit :: Char -> Action State [Lexeme Token]
addStringLit c = inStringLit \s b ->
  do
    setLexerState (InStringLit s (b <> B.singleton c))
    pure []

addStringLitUni :: Action State [Lexeme Token]
addStringLitUni = inStringLit \s b ->
  do
    m <- toEnum . fst . hd . readHex . Text.unpack . Text.drop 2 <$>
         matchText
    setLexerState (InStringLit s (b <> B.singleton m))
    pure []
  where hd xs = case xs of
                  [] -> error "hd"
                  x : _ -> x


addStringLitMatch :: Action State [Lexeme Token]
addStringLitMatch = inStringLit \s b ->
  do
    m <- matchText
    setLexerState (InStringLit s (b <> B.fromText m))
    pure []

endStringLit :: Action State [Lexeme Token]
endStringLit = inStringLit \start b ->
  do
    end <- matchRange
    setLexerState InTop
    pure [ Lexeme {
            lexemeToken = TokString,
            lexemeRange = start <-> end,
            lexemeText = LText.toStrict (B.toLazyText b) } ]
     