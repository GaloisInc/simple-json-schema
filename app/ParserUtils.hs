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
import AST
import PP

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
  case fst (unP parser (pos,lexerAt pos txt)) of
    Right a -> Right a
    Left e -> Left e

data ParseError =
    ParseError SourcePos
  | DuplicateFields [(FieldName,FieldName)]

instance PP ParseError where
  pp err =
    case err of
      ParseError p -> text (prettySourcePosLong p) <.> ":" <+> "parse error"
      DuplicateFields xs ->
        let loc short = text . (if short then prettySourcePos else prettySourcePosLong)
                      . sourceFrom . fieldRange
        in
        vcat [ (loc False x <.> ": Duplicate field")
               $$ nest 2 ("See also:" <+> loc True y)
             | (x,y) <- xs ]

instance Show ParseError where
  show = show . pp

instance Exception ParseError

type RW = (SourcePos, [Lexeme Token])
newtype Parser a = Parser { unP :: RW -> (Either ParseError a, RW) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser \inp -> (Right a, inp)
  (<*>) = ap

instance Monad Parser where
  m >>= f = Parser \inp ->
    case unP m inp of
      (Left err, inp1) -> (Left err, inp1)
      (Right a, inp1) -> unP (f a) inp1

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = Parser \(lastP, inp) ->
  let (t,ts) =
        case inp of
          [] -> (Lexeme { lexemeText = "", lexemeRange = range lastP, lexemeToken = TokEOF }, [])
          tok : toks -> (tok,toks)
      Parser m = k t
  in m (sourceTo (lexemeRange t), ts)

happyError :: Parser a
happyError = Parser \inp -> (Left (ParseError (fst inp)), inp)

parseError :: ParseError -> Parser a
parseError p = Parser \inp -> (Left p, inp)