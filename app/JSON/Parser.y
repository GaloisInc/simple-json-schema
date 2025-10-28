{
module JSON.Parser(value) where

import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Map(Map)
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP(readP_to_S)
import Control.Exception(Exception(..), throwIO)
import Control.Monad(liftM,ap)
import Data.Scientific qualified as Sc
import AlexTools
import Lexer
import ParserUtils
import JSON.AST
}

%tokentype { Lexeme Token }

%token
  '{'       { (matchLexeme -> (TokOpenBrace,    ($$,_))) }
  '}'       { (matchLexeme -> (TokCloseBrace,   ($$,_))) }
  '['       { (matchLexeme -> (TokOpenBracket,  ($$,_))) }
  ']'       { (matchLexeme -> (TokCloseBracket, ($$,_))) }
  ','       { (matchLexeme -> (TokComma,        ($$,_))) }
  ':'       { (matchLexeme -> (TokColon,        ($$,_))) }
  'true'    { (matchLexeme -> (TokKwTrue,       ($$,_))) }
  'false'   { (matchLexeme -> (TokKwFalse,      ($$,_))) }
  'null'    { (matchLexeme -> (TokKwNull,       ($$,_))) }
  NUMBER    { (matchLexeme -> (TokNumber,        $$)) }
  STRING    { (matchLexeme -> (TokString,        $$)) }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }
%name value

%%

value                      :: { JSValue }
  : NUMBER                    {% fmap (\(x,y) -> JSValue x (JSNumber y)) (parseNumber $1) }
  | STRING                    { JSValue (fst $1) (JSString (snd $1)) }
  | 'true'                    { JSValue $1 (JSBool True) }
  | 'false'                   { JSValue $1 (JSBool False) }
  | 'null'                    { JSValue $1 JSNull }
  | '[' values ']'            { JSValue ($1 <-> $3) (JSArray $2) }
  | '{' fields '}'            { JSValue ($1 <-> $3) (JSObject $2) }

values                     :: { [JSValue] }
  : values1                   { reverse $1 }
  | {- empty -}               { [] }

values1                    :: { [JSValue] }
  : value                     { [$1] }
  | values1 ',' value         { $3 : $1 }

fields                     :: { Map Text JSField }
  : fields1                   { $1 }
  | {- empty -}               { mempty }

fields1                    :: { Map Text JSField }
  : field                     { Map.singleton (jsFieldName $1) $1 }
  | fields1 ',' field         {% addField $3 $1 }

field                       :: { JSField }
  : STRING ':' value           { JSField (fst $1) (snd $1) $3 }


{
addField :: JSField -> Map Text JSField -> Parser (Map Text JSField)
addField f mp
  | nm `Map.member` mp  = happyError
  | otherwise           = pure (Map.insert nm f mp)
    where nm = jsFieldName f

}



