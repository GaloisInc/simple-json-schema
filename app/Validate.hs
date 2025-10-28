module Validate where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools
import JSON.AST
import AST

data ValidationError =
    Mismatch SourceRange (Type Name) JSValue
  | TupleLengthMismatch SourceRange Int Int  -- ^ Expected, actual
  | OrMismatch [ValidationError] [ValidationError]
  | MissingField SourceRange FieldName
  | UnexpectedField SourceRange JSField

type Defs = Map Name (Decl Name Name)

validateDecl :: Defs -> Name -> JSValue -> [ValidationError]
validateDecl defs x val =
  case Map.lookup x defs of
    Nothing -> error ("[BUG]: Missing definition for " ++ show x)
    Just decl -> validateType defs (declRange decl) (declDef decl) val

validateType :: Defs -> SourceRange -> Type Name -> JSValue -> [ValidationError]
validateType defs r ty val =
  case ty of
    TExact v -> validateValue r v val

    t1 :| t2 ->
      case (validateType defs r t1 val, validateType defs r t2 val) of
        ([],_) -> []
        (_,[]) -> []
        (es1,es2) -> [OrMismatch es1 es2]

    TNumber ->
      case jsValue val of
        JSNumber {} -> []
        _ -> mismatch

    TBoolean ->
      case jsValue val of
        JSBool {} -> []
        _ -> mismatch

    TString ->
      case jsValue val of
        JSString {} -> []
        _ -> mismatch

    TObject os ->
      case jsValue val of
        JSObject fs -> validateObject defs (jsRange val) os fs
        _ -> mismatch

    TArray t ->
      case jsValue val of
        JSArray arr -> concatMap (validateType defs r t) arr
        _ -> mismatch

    TTuple ts ->
      case jsValue val of
        JSArray arr
          | expected == actual -> go
          | otherwise -> TupleLengthMismatch r expected actual : go
            where
            expected = length ts
            actual   = length arr
            go = concat (zipWith (validateType defs r) ts arr)
        _ -> mismatch

    TNamed nm -> validateDecl defs nm val

    TLocated r1 t -> validateType defs r1 t val

    TAny -> []
  where
  mismatch = [ Mismatch r ty val ]


validateValue :: SourceRange -> Value -> JSValue -> [ValidationError]
validateValue r v val =
  case (v,jsValue val) of
    (VNull,      JSNull)                 -> []
    (VBool b1,   JSBool b2)   | b1 == b2 -> []
    (VNumber n1, JSNumber n2) | n1 == n2 -> []
    (VString t1, JSString t2) | t1 == t2 -> []
    _                                     -> [ Mismatch r (TExact v) val ]

validateObject :: Defs -> SourceRange -> [Field Name] -> Map Text JSField -> [ValidationError]
validateObject defs rng fs obj
  | not (null unused) && not ok = [ ] ++ objErrs
  | otherwise = objErrs
  where
  (unused, ok, objErrs) = foldl validateField (obj,False,[]) fs
  validateField (mp,extraOk,errs) f =
    case f of
      OtherFields -> (mp,True,errs)
      x :> t ->
        case Map.lookup nm mp of
          Nothing | fieldRequired x -> (mp,extraOk, MissingField rng x : errs)
                  | otherwise -> (mp,extraOk,errs)
          Just v -> (Map.delete nm mp, extraOk, validateType defs (fieldRange x) t (jsFieldValue v) ++ errs)
        where nm = fieldName x
