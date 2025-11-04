{
module Parser(spec) where

import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.ParserCombinators.ReadP(readP_to_S)
import Data.Map qualified as Map
import Control.Exception(Exception(..), throwIO)
import Control.Monad(liftM,ap)
import Data.Scientific qualified as Sc
import AlexTools
import ParserUtils
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
  '...'     { (matchLexeme -> (TokDots,         ($$,_))) }
  '='       { (matchLexeme -> (TokEqual,        ($$,_))) }
  'import'  { (matchLexeme -> (TokKwImport,     ($$,_))) }
  'type'    { (matchLexeme -> (TokKwType,       ($$,_))) }
  'from'    { (matchLexeme -> (TokKwFrom,       ($$,_))) }
  'as'      { (matchLexeme -> (TokKwAs,         ($$,_))) }
  'true'    { (matchLexeme -> (TokKwTrue,       ($$,_))) }
  'false'   { (matchLexeme -> (TokKwFalse,      ($$,_))) }
  'null'    { (matchLexeme -> (TokKwNull,       ($$,_))) }
  'string'  { (matchLexeme -> (TokKwString,     ($$,_))) }
  'number'  { (matchLexeme -> (TokKwNumber,     ($$,_))) }
  'boolean' { (matchLexeme -> (TokKwBoolean,    ($$,_))) }
  'any'     { (matchLexeme -> (TokKwAny,        ($$,_))) }
  COMMENT   { (matchLexeme -> (TokDocComment,   $$)) }
  IDENT     { (matchLexeme -> (TokIdent,        $$)) }
  NUMBER    { (matchLexeme -> (TokNumber  ,     $$)) }
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
  : 'import' importSpec 'from' STRING 
                               { Import $2 (snd $4) (-1) ($1 <-> fst $4) }

importSpec                  :: { ImportSpec }
  : '*' 'as' IDENT             { ImportAll (snd $3) }
  | '{' names '}'              { ImportList $2 }

names1                      :: { [Text] }
  : IDENT                      { [snd $1] }
  | names1 ',' IDENT           { snd $3 : $1 }

names                       :: { [Text] }
  : names1                     { reverse $1 }
  | {- empty -}                { [] }

qname                       :: { (SourceRange, QName) }
  : IDENT                      { (fst $1, Unqual (snd $1)) }
  | IDENT '.' IDENT            { (fst $1 <-> fst $3, Qual (snd $1) (snd $3)) }


comments                    :: { [Text] }
  : comments1                  { reverse $1 }
  | {- empty -}                { [] }

comments1                   :: { [Text] }
  : comment                    { [$1] }
  | comments1 comment          { $2 : $1 }

comment                     :: { Text }
  : COMMENT                    { Text.drop 3 (snd $1) }

decls                       :: { [Decl Text QName] }
  : decls decl                 { $2 : $1 }
  | {- empty -}                { [] }

decl                        :: { Decl Text QName }
  : comments 'type' IDENT '=' type
                              { Decl { declName = snd $3, declDef = $5, declRange = fst $3, declDocs = $1 } }

type                        :: { Type QName }
  : atype                      { $1 }
  | type '|' type              { TLocated (fromMaybe $2 (typeRange $1) <-> fromMaybe $2 (typeRange $3)) (orTypes $1 $3) }

atype                       :: { Type QName }
  : qname                      { uncurry TLocated (fmap TNamed $1) }
  | value                      { uncurry TLocated (fmap TExact $1) }
  | 'number'                   { TLocated $1 (TBuiltIn TNumber) }
  | 'boolean'                  { TLocated $1 (TBuiltIn TBoolean) }
  | 'string'                   { TLocated $1 (TBuiltIn TString) }
  | 'any'                      { TLocated $1 (TBuiltIn TAny) }
  | '{' fields '}'             { TLocated ($1 <-> $3) (TObject (MatchFields $2)) }
  | '[' types ']'              { TLocated ($1 <-> $3) (TTuple $2) }
  | '(' type ')'               { $2 }
  | atype '[' ']'              { TLocated (fromMaybe $2 (typeRange $1) <-> $3) (TArray $1) }
  | atype '?'                  { TLocated (fromMaybe $2 (typeRange $1) <-> $2) ($1 :| TExact VNull) }

value                        :: { (SourceRange, Value) }
  : NUMBER                      {% fmap (fmap VNumber) (parseNumber $1) }
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

field_text                   :: { (SourceRange, Text) }
  : IDENT                       { $1 }
  | STRING                      { $1 }
  | 'import'                    { ($1, "import") }
  | 'type'                      { ($1, "type") }
  | 'from'                      { ($1, "from") }
  | 'as'                        { ($1, "as") }
  | 'true'                      { ($1, "true") }
  | 'false'                     { ($1, "false") }
  | 'null'                      { ($1, "null") }
  | 'string'                    { ($1, "string") }
  | 'number'                    { ($1, "number") }
  | 'boolean'                   { ($1, "boolean") }
  | 'any'                       { ($1, "any") }

field_name                   :: { FieldName }
  : comments field_text         { FieldName (snd $2) True (fst $2) $1 }

field_name_opt               :: { FieldName }
  : field_name                  { $1 }
  | field_name '?'              { $1 { fieldRequired = False } }

field                        :: { FieldSpecAnd QName }
  : field_name_opt ':' type     { FieldSpecAnd (Map.singleton (fieldName $1) ($1, $3)) False }
  | '...'                       { FieldSpecAnd Map.empty True }

fields1                      :: { FieldSpecAnd QName }
  : field                       { $1  }
  | fields1 ',' field           {% case andFields $1 $3 of Left es -> parseError (DuplicateFields es); Right a -> pure a }

fields                       :: { FieldSpecAnd QName }
  : fields1                     { $1 }
  | {- empty -}                 { FieldSpecAnd Map.empty False }



