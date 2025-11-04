{
module Lexer (
  Token(..),
  lexer,
  lexerAt,
  skipComments,
  Lexeme(..),
  SourceRange(..),
  SourcePos(..)
) where

import Data.Text(Text)
import AlexTools
import LexerUtils
}

$idchar         = [a-zA-Z_]
$digit          = 0-9
$hex_digit      = [$digit a-f A-F]
$char           = [^\"\\]
@block_comment  = "/*" ([^\*]| "*" [^\/])* "*/"
@number         = "-"? (0 | [1-9]$digit*) ("." $digit+)? ([Ee] [\+\-]? $digit+)?
@ident          = $idchar+ [$idchar $digit]*
@esc            = [\"\\\/bfnrt] | u$hex_digit{4}

:-

<0> {
\" $char* \"    { simpleStringLit }
\"              { startStringLit }
"("             { lexeme TokOpenParen }
")"             { lexeme TokCloseParen }
"{"             { lexeme TokOpenBrace }
"}"             { lexeme TokCloseBrace }
"["             { lexeme TokOpenBracket }
"]"             { lexeme TokCloseBracket }
","             { lexeme TokComma }
":"             { lexeme TokColon }
"."             { lexeme TokDot }
"..."           { lexeme TokDots }
"|"             { lexeme TokBar }
"?"             { lexeme TokQuestion }
"*"             { lexeme TokStar }
"="             { lexeme TokEqual }
"import"        { lexeme TokKwImport }
"type"          { lexeme TokKwType }
"from"          { lexeme TokKwFrom }
"as"            { lexeme TokKwAs }
"true"          { lexeme TokKwTrue }
"false"         { lexeme TokKwFalse }
"null"          { lexeme TokKwNull }
"string"        { lexeme TokKwString }
"number"        { lexeme TokKwNumber }
"boolean"       { lexeme TokKwBoolean }
"any"           { lexeme TokKwAny }
"///" .*        { lexeme TokDocComment }
"//" .*         { lexeme TokComment }
@block_comment  { lexeme TokComment }
@number         { lexeme TokNumber }
@ident          { lexeme TokIdent }
\n              ;
$white          ;
.               { lexeme TokError }
}

<string> {
\"                  { endStringLit }
\\ \\               { addStringLit '\\' }
\/                  { addStringLit '/' }
\\ b                { addStringLit '\b' }
\\ f                { addStringLit '\f' }
\\ n                { addStringLit '\n' }
\\ r                { addStringLit '\r' }
\\ t                { addStringLit '\t' }
\\ U $hex_digit{4}  { addStringLitUni }
$char+              { addStringLitMatch }

}

{

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = makeAlexGetByte \c ->
  let n = fromEnum c
  in if n < 128 then toEnum n else 0

skipComments :: [Lexeme Token] -> [Lexeme Token]
skipComments = filter \x ->
  case lexemeToken x of
    TokComment -> False
    _ -> True 


lexer :: Text -> Text -> [Lexeme Token]
lexer file = lexerAt (startPos file)

lexerAt :: SourcePos -> Text -> [Lexeme Token]
lexerAt loc txt = $makeLexer cfg (initialInputAt loc txt)
  where
  cfg = LexerConfig {
    lexerInitialState = InTop,
    lexerStateMode = \case
      InTop -> 0
      InStringLit {} -> string,
    lexerEOF = \case
      InTop -> \p -> [Lexeme { lexemeText = "", lexemeToken = TokEOF, lexemeRange = range p }]
      InStringLit p _ ->
        \_ -> [Lexeme { lexemeText = "Unterminated string literal", lexemeToken = TokError, lexemeRange = range p }]
  }
}