module Validate where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.List(foldl')
import AlexTools
import JSON.AST
import AST
import PP

data ValidationError = ValidataionError {
  jsonRange   :: SourceRange,
  schemaRange :: SourceRange,
  problem     :: ValidationProblem
}

data ValidationProblem =
    ShapeMismatch (Type Name) JSValue
  | TupleLengthMismatch Int Int  -- ^ Expected, actual
  | MissingField Text
  | UnexpectedField Text

instance PP ValidationError where
  pp err =
    text (prettySourceRangeLong (jsonRange err)) <.> ":" <+>
    pp (problem err) $$ nest 2 ("See schema:" <+> text (prettySourceRangeLong (schemaRange err)))
    
instance PP ValidationProblem where
  pp p =
    case p of
      ShapeMismatch t j -> "JSON value shape does not match schema" $$ nest 2 (vcat [pp t, pp j])
      TupleLengthMismatch e a ->
        "Invalid array length:" $$ nest 2 (vcat [ "Expected:" <+> int e, "Actual:" <+> int a ])
      MissingField t -> "Object is missing required filed:" <+> text (show t)
      UnexpectedField t -> "Object has an additional field:" <+> text (show t)

type Defs = Map Name (Decl Name Name)

data ValidataionResult =
    Mismatch [ValidationError]
    -- ^ The basic value shapes are not matched
  | PartialMatch Int [ValidationError]
    -- ^ Value does not match spec, but some parts of it do.
    -- Bigger number means match more, and we prefer these
    -- when reporting errors
  | Match -- Value matches spec


validateDecl :: Defs -> Name -> JSValue -> ValidataionResult
validateDecl defs x val =
  case Map.lookup x defs of
    Nothing -> error ("[BUG]: Missing definition for " ++ show x)
    Just decl -> validateType defs (declRange decl) (declDef decl) val

validateType :: Defs -> SourceRange -> Type Name -> JSValue -> ValidataionResult
validateType defs r ty val =
  case ty of
    TExact v -> validateValue r v val

    TBuiltIn t -> validateBuiltIn r t val

    t1 :| t2 ->
      case (validateType defs r t1 val, validateType defs r t2 val) of
        (Match, _) -> Match
        (_, Match) -> Match
        (Mismatch {}, Mismatch {}) -> Mismatch mismatch
        (lhs, Mismatch {}) -> lhs
        (Mismatch {}, rhs) -> rhs
        (lhs@(PartialMatch x as), rhs@(PartialMatch y bs)) ->
          case compare x y of
            LT -> rhs
            GT -> lhs
            EQ -> PartialMatch x (as ++ bs)

    TObject os ->
      case jsValue val of
        JSObject fs -> validateObject defs r (jsRange val) os fs
        _ -> Mismatch mismatch

    TArray t ->
      case jsValue val of
        JSArray arr -> validateMany defs r (zip (repeat t) arr)
        _ -> Mismatch mismatch

    TTuple ts ->
      case jsValue val of
        JSArray arr
          | expected == actual -> validateMany defs r (zip ts arr)
          | otherwise -> Mismatch [err (TupleLengthMismatch expected actual)]
            where
            expected = length ts
            actual   = length arr

        _ -> Mismatch mismatch

    TNamed nm -> validateDecl defs nm val

    TLocated r1 t -> validateType defs r1 t val

  where
  err p =
    ValidataionError {
      jsonRange = jsRange val,
      schemaRange = r,
      problem = p
    }
  mismatch = [ err (ShapeMismatch ty val) ]


validateBuiltIn :: SourceRange -> BuiltIn -> JSValue -> ValidataionResult
validateBuiltIn r ty val =
  case ty of
    TNumber ->
      case jsValue val of
        JSNumber {} -> Match
        _ -> mismatch

    TBoolean ->
      case jsValue val of
        JSBool {} -> Match
        _ -> mismatch

    TString ->
      case jsValue val of
        JSString {} -> Match
        _ -> mismatch

    TAny -> Match
    
  where
  mismatch =
    Mismatch [ ValidataionError {
                 jsonRange = jsRange val,
                 schemaRange = r,
                 problem = ShapeMismatch (TBuiltIn ty) val
               }
             ]



validateValue :: SourceRange -> Value -> JSValue -> ValidataionResult
validateValue r v val =
  case (v,jsValue val) of
    (VNull,      JSNull)      -> Match
    (VBool b1,   JSBool b2)   -> check b1 b2
    (VNumber n1, JSNumber n2) -> check n1 n2
    (VString t1, JSString t2) -> check t1 t2
    _ -> Mismatch mismatch
  where
  check x y = if x == y then Match else PartialMatch 0 mismatch
  mismatch = [ ValidataionError {
                 jsonRange = jsRange val,
                 schemaRange = r,
                 problem = ShapeMismatch (TExact v) val
               }
             ]

validateMany :: Defs -> SourceRange -> [(Type Name, JSValue)] -> ValidataionResult
validateMany defs r goals =
  case errs of
    [] -> Match
    _  -> PartialMatch pscore errs
  where
  (pscore,errs) = foldl' check (0,[]) goals
  check (score,es0) (t,v) =
    case validateType defs r t v of
      Match             -> (score + 2, es0)
      PartialMatch _ es -> (score + 1, es ++ es0)
      Mismatch es       -> (score, es ++ es0)

validateObject :: Defs -> SourceRange -> SourceRange -> [Field Name] -> Map Text JSField -> ValidataionResult
validateObject defs srng orng fs obj
  | not (null unused) && not ok =
    PartialMatch (max 0 (pscore - length badFields)) (badFields ++ objErrs)
  | otherwise =
    case objErrs of
      [] -> Match
      _ -> PartialMatch pscore objErrs
  where
  badFields = [ ValidataionError {
                  jsonRange = jsFieldRange f,
                  schemaRange = srng,
                  problem = UnexpectedField (jsFieldName f)
                }
              | f <- Map.elems unused
              ]
  (unused, ok, pscore, objErrs) = foldl' validateField (obj,False,0,[]) fs
  validateField (mp,extraOk,score,errs) f =
    case f of
      OtherFields -> (mp,True,score,errs)
      x :> t ->
        case Map.lookup nm mp of
          Nothing | fieldRequired x -> (mp,extraOk,score-1,
                    ValidataionError {
                      jsonRange = orng,
                      schemaRange = fieldRange x,
                      problem = MissingField (fieldName x)
                      } : errs)
                  | otherwise -> (mp,extraOk,score,errs)
          Just v ->
            let 
                (newScore,newErrs) =
                  case validateType defs (fieldRange x) t (jsFieldValue v) of
                    Match               -> (score + 3, errs)
                    Mismatch es         -> (score + 1, es ++ errs)
                    PartialMatch _n es  -> (score + 2, es ++ errs)
            in (Map.delete nm mp, extraOk, newScore, newErrs)
        where nm = fieldName x
