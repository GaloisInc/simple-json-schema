{
module Parser(
  parseSpecAt,
  parseSpec,
  parseSpecFromFile,
  ParseError(..),
  SourceRange(..),
  SourcePos(..)
 ) where

import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Control.Exception(Exception(..), throwIO)
import Control.Monad(liftM,ap)
import AlexTools
import Lexer
import AST
}

%tokentype { Lexeme Token }

%token
  '('       { (matchLexeme -> (TokOpenParen,    ($$,_))) }
  ')'       { (matchLexeme -> (TokCloseParen,   ($$,_))) }
  '{'       { (matchLexeme -> (TokOpenBrace,    ($$,_))) }
  '}'       { (matchLexeme -> (TokCloseBrace,   ($$,_))) }
  '['       { (matchLexeme -> (TokOpenBracket,  ($$,_))) }
  ']'       { (matchLexeme -> (TokCloseBracket, ($$,_))) }
  ','       { (matchLexeme -> (TokComma,        ($$,_))) }
  ':'       { (matchLexeme -> (TokColon,        ($$,_))) }
  '|'       { (matchLexeme -> (TokBar,          ($$,_))) }
  '?'       { (matchLexeme -> (TokQuestion,     ($$,_))) }
  '*'       { (matchLexeme -> (TokStar,         ($$,_))) }
  '.'       { (matchLexeme -> (TokDot,          ($$,_))) }
  'import'  { (matchLexeme -> (TokKwImport,     ($$,_))) }
  'from'    { (matchLexeme -> (TokKwFrom,       ($$,_))) }
  'as'      { (matchLexeme -> (TokKwAs,         ($$,_))) }
  'true'    { (matchLexeme -> (TokKwTrue,       ($$,_))) }
  'false'   { (matchLexeme -> (TokKwFalse,      ($$,_))) }
  'null'    { (matchLexeme -> (TokKwNull,       ($$,_))) }
  'string'  { (matchLexeme -> (TokKwString,     ($$,_))) }
  'number'  { (matchLexeme -> (TokKwNumber,     ($$,_))) }
  'boolean' { (matchLexeme -> (TokKwBoolean,    ($$,_))) }
  IDENT     { (matchLexeme -> (TokIdent,        $$)) }
  UNSIGNED  { (matchLexeme -> (TokUnsigned,     $$)) }
  STRING    { (matchLexeme -> (TokString,       $$)) }

%left '|'

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }
%name spec

%%

spec                       :: { Module }
  : imports decls             { Module (reverse $1) (reverse $2) }

imports                    :: { [Import] }
  : imports import            { $2 : $1 }
  | {- empty -}               { [] }

import                      :: { Import }
  : 'import' importSpec importAs 'from' STRING 
                               { Import $2 $3 (snd $5) (-1) ($1 <-> fst $5) }

importSpec                  :: { ImportSpec }
  : '*'                        { ImportAll }
  | '{' names '}'              { ImportList $2 }

importAs                    :: { Maybe Text }
  : 'as' IDENT                 { Just (snd $2) }
  | {- empty -}                { Nothing }  

names1                      :: { [Text] }
  : IDENT                      { [snd $1] }
  | names1 ',' IDENT           { snd $3 : $1 }

names                       :: { [Text] }
  : names1                     { reverse $1 }
  | {- empty -}                { [] }

qname                       :: { (SourceRange, QName) }
  : IDENT                      { (fst $1, Unqual (snd $1)) }
  | IDENT '.' IDENT            { (fst $1 <-> fst $3, Qual (snd $1) (snd $3)) }


decls                       :: { [Decl Text QName] }
  : decls decl                 { $2 : $1 }
  | {- empty -}                { [] }

decl                        :: { Decl Text QName }
  : IDENT ':' type             { Decl { declName = snd $1, declDef = $3, declRange = fst $1 }    }

type                        :: { Type QName }
  : atype                      { $1 }
  | type '|' type              { TLocated (fromMaybe $2 (typeRange $1) <-> fromMaybe $2 (typeRange $3)) ($1 :| $3) }

atype                       :: { Type QName }
  : qname                      { uncurry TLocated (fmap TNamed $1) }
  | value                      { uncurry TLocated (fmap TExact $1) }
  | 'number'                   { TLocated $1 TNumber }
  | 'boolean'                  { TLocated $1 TBoolean }
  | 'string'                   { TLocated $1 TString }
  | '{' fields '}'             { TLocated ($1 <-> $3) (TObject $2) }
  | '[' types ']'              { TLocated ($1 <-> $3) (TTuple $2) }
  | '(' type ')'               { $2 }
  | atype '[' ']'              { TLocated (fromMaybe $2 (typeRange $1) <-> $3) (TArray $1) }
  | atype '?'                  { TLocated (fromMaybe $2 (typeRange $1) <-> $2) ($1 :| TExact VNull) }

value                        :: { (SourceRange, Value) }
  : UNSIGNED                    { fmap VInt $1 }
  | STRING                      { fmap VString $1 }
  | 'true'                      { ($1, VBool True) }
  | 'false'                     { ($1, VBool False) }
  | 'null'                      { ($1, VNull) }

types1                       :: { [Type QName] }
  : type                        { [$1] }
  | types1 ',' type             { $3 : $1 }

types                        :: { [Type QName] }
  : types1                      { reverse $1 }
  | {- empty -}                 { [] }

field_name                   :: { FieldName }
  : IDENT                       { FieldName (snd $1) True (fst $1) }
  | STRING                      { FieldName (snd $1) True (fst $1) }

field_name_opt               :: { FieldName }
  : field_name                  { $1 }
  | field_name '?'              { $1 { fieldRequired = False } }

field                        :: { Field QName }
  : field_name_opt ':' type     { $1 :> $3 }

fields1                      :: { [Field QName ] }
  : field                       { [$1] }
  | fields1 ',' field           { $3 : $1 }

fields                       :: { [Field QName] }
  : fields1                     { reverse $1 }
  | {- empty -}                 { [] }



{
matchLexeme :: Lexeme Token -> (Token, (SourceRange,Text))
matchLexeme l = (lexemeToken l, (lexemeRange l, lexemeText l))

newtype ParseError = ParseError SourcePos

instance Show ParseError where
  show (ParseError p) = "Parse error at " ++ show p

instance Exception ParseError

-- | Throws `ParseError` on parser error
parseSpecFromFile :: FilePath -> IO Module
parseSpecFromFile file =
  do txt <- Text.readFile file
     case parseSpec (Text.pack file) txt of
       Left err -> throwIO err
       Right a -> pure a

parseSpec :: Text -> Text -> Either ParseError Module
parseSpec file = parseSpecAt (startPos file)

parseSpecAt :: SourcePos -> Text -> Either ParseError Module
parseSpecAt pos txt =
  case unP spec (pos,lexerAt pos txt) of
    (Just a, _) -> Right a
    (Nothing, (p,_)) -> Left (ParseError p)

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
          t : ts -> (t,ts)
      Parser m = k t
  in m (sourceTo (lexemeRange t), ts)

happyError :: Parser a
happyError = Parser \inp -> (Nothing, inp)
}