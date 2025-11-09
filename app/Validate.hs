module Validate where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools
import JSON.Lexer qualified as JS
import JSON.AST
import AST
import PP

data ValidationError = ValidataionError {
  jsonRange   :: JS.SourceRange,
  schemaRange :: [SourceRange],
  problem     :: ValidationProblem
}

data ValidationProblem =
    ShapeMismatch (Type Name) JSValue
  | TupleLengthMismatch Int Int  -- ^ Expected, actual
  | MissingField Text
  | UnexpectedField Text

instance PP ValidationError where
  pp err =
    pp (jsonRange err) <.> ":" <+>
    pp (problem err) $$
      nest 2 ("See schema:" $$
        nest 2 (vcat [ text (prettySourceRangeLong r) | r <- schemaRange err ]))
    
instance PP ValidationProblem where
  pp p =
    case p of

      ShapeMismatch t j -> "JSON value shape does not match schema" $$
        nest 2 (vcat [pp t, pp j])

      TupleLengthMismatch e a ->
        "Invalid array length:" $$
          nest 2 (vcat [ "Expected:" <+> int e, "Actual:" <+> int a ])

      MissingField t -> "Object is missing required field:" <+> text (show t)

      UnexpectedField t -> "Object has an additional field:" <+> text (show t)

type Defs = Map Name (Decl Name Name)

type ValidataionResult = [ValidationError]

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
        ([], _) -> []
        (_, []) -> []
        (xs, ys) -> xs ++ ys -- XXX

    TObject os ->
      case jsValue val of
        JSObject fs -> validateFieldSpec defs [r] (jsRange val) os fs
        _ -> mismatch

    TArray t ->
      case jsValue val of
        JSArray arr -> validateMany defs r (zip (repeat t) arr)
        _ -> mismatch

    TTuple ts ->
      case jsValue val of
        JSArray arr
          | expected == actual -> validateMany defs r (zip ts arr)
          | otherwise -> [err (TupleLengthMismatch expected actual)]
            where
            expected = length ts
            actual   = length arr

        _ -> mismatch

    TNamed nm -> validateDecl defs nm val

    TLocated r1 t -> validateType defs r1 t val

  where
  err p =
    ValidataionError {
      jsonRange = jsRange val,
      schemaRange = [r],
      problem = p
    }
  mismatch = [ err (ShapeMismatch ty val) ]

validateFieldSpec ::
  Defs ->
  [SourceRange] ->
  JS.SourceRange ->
  FieldSpec Name ->
  Map Text JSField ->
  ValidataionResult
validateFieldSpec defs srng rng s fs =
  case s of
    CaseField f ks ->
      case Map.lookup f fs of
        Nothing -> [mkError srng (MissingField f)]
        Just v ->
          case toValue (jsFieldValue v) of
            Just yes | Just (srng1,k) <- Map.lookup yes ks ->
              validateFieldSpec defs srng1 (jsFieldRange v) k (Map.delete f fs)
            _ -> [mkError srng (ShapeMismatch ty (jsFieldValue v))]
              where ty = foldr1 (:|) (map TExact (Map.keys ks))
    MatchFields fsSpec -> validateFieldSpecAnd defs srng rng fsSpec fs
    OrFields xs ys ->
      case (validateFieldSpec defs srng rng xs fs,
            validateFieldSpec defs srng rng ys fs) of
        ([],_) -> []
        (_,[]) -> []
        (es1,es2) -> es1 ++ es2 -- XXX
  where
  mkError r p = ValidataionError {
    jsonRange = rng,
    schemaRange = r,
    problem = p
  }

toValue :: JSValue -> Maybe Value
toValue js =
  case jsValue js of
    JSNull -> Just VNull
    JSBool b -> Just (VBool b)
    JSNumber n -> Just (VNumber n)
    JSString s -> Just (VString s)
    _ -> Nothing

validateFieldSpecAnd :: Defs -> [SourceRange] -> JS.SourceRange -> FieldSpecAnd Name -> Map Text JSField -> ValidataionResult
validateFieldSpecAnd defs srng jrng fspecAnd fs
  | null shapeErrs = concat (Map.elems (Map.intersectionWith checkF fspec fs))
  | otherwise = shapeErrs
  where
  checkF (f,t) j = validateType defs (fieldRange f) t (jsFieldValue j)

  fspec   = matchFields fspecAnd
  shapeErrs = missing ++ extra
  missing =
    [ ValidataionError {
      jsonRange = jrng,
      schemaRange = [fieldRange f],
      problem = MissingField (fieldName f)
      }
    | (f,_) <- Map.elems (Map.difference fspec fs)
    , fieldRequired f
    ]
  extra
    | otherFieldsOk fspecAnd = []
    | otherwise = [
      ValidataionError {
        jsonRange   = jsFieldRange j,
        schemaRange = srng,
        problem = UnexpectedField (jsFieldName j)
      }
      | j <- Map.elems (Map.difference fs fspec)
      ]

validateBuiltIn :: SourceRange -> BuiltIn -> JSValue -> ValidataionResult
validateBuiltIn r ty val =
  case ty of
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

    TAny -> []
    
  where
  mismatch =
    [ ValidataionError {
        jsonRange = jsRange val,
        schemaRange = [r],
        problem = ShapeMismatch (TBuiltIn ty) val
      }
    ]



validateValue :: SourceRange -> Value -> JSValue -> ValidataionResult
validateValue r v val =
  case (v,jsValue val) of
    (VNull,      JSNull)      -> []
    (VBool b1,   JSBool b2)   -> check b1 b2
    (VNumber n1, JSNumber n2) -> check n1 n2
    (VString t1, JSString t2) -> check t1 t2
    _ -> mismatch
  where
  check x y = if x == y then [] else mismatch
  mismatch = [ ValidataionError {
                 jsonRange = jsRange val,
                 schemaRange = [r],
                 problem = ShapeMismatch (TExact v) val
               }
             ]

validateMany :: Defs -> SourceRange -> [(Type Name, JSValue)] -> ValidataionResult
validateMany defs r goals = [ e | (t,v) <- goals, e <- validateType defs r t v ]
  