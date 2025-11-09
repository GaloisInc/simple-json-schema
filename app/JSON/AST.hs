module JSON.AST where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Text(Text)
import Data.Scientific(Scientific)
import PP
import JSON.Lexer(SourceRange(..))


data JSValue = JSValue {
  jsRange :: !SourceRange,
  jsValue :: !JSValueShape
}

data JSValueShape =
    JSNull
  | JSBool !Bool
  | JSString !Text
  | JSNumber !Scientific
  | JSArray ![JSValue]
  | JSObject !(Map Text JSField)

data JSField = JSField {
  jsFieldRange :: !SourceRange,
  jsFieldName  :: Text,
  jsFieldValue :: JSValue
}

instance PP JSValue where
  pp = pp . jsValue

instance PP JSValueShape where
  pp val =
    case val of
      JSNull -> "null"
      JSBool b -> if b then "true" else "false"
      JSString t -> text (show t) -- XXX: escapes
      JSNumber s -> text (show s) -- XXX: format?
      JSArray xs
        | null xs -> "[]"
        | all isSmall xs -> brackets (fsep ds)
        | otherwise -> "[" $$ nest 2 (vcat ds) $$ "]"
          where ds = punctuate comma (map pp xs)
      JSObject xs
        | null xs -> "{}"
        | all (isSmall . jsFieldValue) xs -> braces (fsep ds)
        | otherwise -> "{" $$ nest 2 (vcat ds) $$ "}"
          where ds = punctuate comma (map pp (Map.elems xs))
          
    where
    isSmall v =
      case jsValue v of
        JSArray fs -> null fs
        JSObject fs -> null fs
        _ -> True

instance PP JSField where
  pp f = text (show (jsFieldName f)) <.> ":" <+> pp (jsFieldValue f)