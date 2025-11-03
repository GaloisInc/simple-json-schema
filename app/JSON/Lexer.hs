module JSON.Lexer
  ( lexerAt, Token(..)
  ) where

import Data.Text(Text)
import AlexTools
import LexerUtils(Token)
import Lexer qualified as L

lexerAt :: SourcePos -> Text -> [Lexeme Token]
lexerAt = L.lexerAt

