module JSON.AesonParser where

import JSON.Lexer
import JSON.AST
import Data.Aeson qualified as JS
import Data.Aeson.KeyMap qualified as JS
import Data.Vector qualified as V
import Data.Map qualified as Map

parseFromFile :: FilePath -> IO JSValue
parseFromFile fp =
  do
    res <- JS.eitherDecodeFileStrict' fp
    case res of
      Right a -> pure (toJSValue a)
      Left err -> fail err

dummy :: SourceRange
dummy = SourceRange 0 0

toJSValue :: JS.Value -> JSValue
toJSValue v =
  JSValue {
    jsRange = dummy,
    jsValue = toJSValueShape v
  }

toJSValueShape :: JS.Value -> JSValueShape
toJSValueShape v =
  case v of
    JS.Null -> JSNull
    JS.Bool b -> JSBool b
    JS.Number x -> JSNumber x
    JS.String x -> JSString x
    JS.Array xs -> JSArray (V.toList (toJSValue <$> xs))
    JS.Object xs -> JSObject (Map.mapWithKey mk (JS.toMapText xs))
      where
      mk x y = JSField {
        jsFieldRange = dummy,
        jsFieldName = x,
        jsFieldValue = toJSValue y
      }



