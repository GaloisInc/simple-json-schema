module JSON.AST where

import Data.Map(Map)
import Data.Text(Text)
import AlexTools
import Data.Scientific(Scientific)

data JSValue = JSValue {
  jsRange :: SourceRange,
  jsValue :: JSValueShape
}

data JSValueShape =
    JSNull
  | JSBool Bool
  | JSString Text
  | JSNumber Scientific
  | JSArray [JSValue]
  | JSObject (Map Text JSField)

data JSField = JSField {
  jsFieldRange :: SourceRange,
  jsFieldName  :: Text,
  jsFieldValue :: JSValue
}