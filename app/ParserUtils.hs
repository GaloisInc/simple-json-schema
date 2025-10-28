module ParserUtils where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Scientific(Scientific,scientificP)
import Text.ParserCombinators.ReadP(readP_to_S)
import Control.Exception(Exception,throwIO)
import Control.Monad(liftM,ap)
import LexerUtils
import AlexTools
import Lexer

matchLexeme :: Lexeme Token -> (Token, (SourceRange, Text))
matchLexeme l = (lexemeToken l, (lexemeRange l, lexemeText l))

parseNumber :: (SourceRange, Text) -> Parser (SourceRange, Scientific)
parseNumber (rng, txt) =
  case readP_to_S scientificP (Text.unpack txt) of
    [(x,"")] -> pure (rng, x)
    _ -> happyError


-- | Throws `ParseError` on parser error
parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile p file =
  do txt <- Text.readFile file
     case parse p (Text.pack file) txt of
       Left err -> throwIO err
       Right a -> pure a

parse :: Parser a -> Text -> Text -> Either ParseError a
parse p file = parseAt p (startPos file)

parseAt :: Parser a -> SourcePos -> Text -> Either ParseError a
parseAt parser pos txt =
  case unP parser (pos,lexerAt pos txt) of
    (Just a, _) -> Right a
    (Nothing, (p,_)) -> Left (ParseError p)

newtype ParseError = ParseError SourcePos

instance Show ParseError where
  show (ParseError p) = "Parse error at " ++ show p

instance Exception ParseError


type RW = (SourcePos, [Lexeme Token])
newtype Parser a = Parser { unP :: RW -> (Maybe a, RW) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser \inp -> (Just a, inp)
  (<*>) = ap

instance Monad Parser where
  m >>= f = Parser \inp ->
    case unP m inp of
      (Nothing, inp1) -> (Nothing, inp1)
      (Just a, inp1) -> unP (f a) inp1

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = Parser \(lastP, inp) ->
  let (t,ts) =
        case inp of
          [] -> (Lexeme { lexemeText = "", lexemeRange = range lastP, lexemeToken = TokEOF }, [])
          tok : toks -> (tok,toks)
      Parser m = k t
  in m (sourceTo (lexemeRange t), ts)

happyError :: Parser a
happyError = Parser \inp -> (Nothing, inp)