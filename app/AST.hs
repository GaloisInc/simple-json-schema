module AST where
  
import Data.Text(Text)
import Data.List(intersperse)
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools(SourceRange)
import Data.Scientific(Scientific)
import PP

data Module = Module {
  moduleImports :: [Import],
  moduleDecls   :: [Decl Text QName]
} deriving Show

data Import = Import {
  importSpec  :: ImportSpec,
  importFile  :: Text,
  importId    :: Int, -- ^ File Id, filled in by Resolve
  importRange :: SourceRange
} deriving Show

data ImportSpec =
    ImportAll Text
  | ImportList [Text]
    deriving Show

data QName =
    Qual Text Text
  | Unqual Text
    deriving (Show, Eq, Ord)

instance PP QName where
  pp q =
    case q of
      Unqual x -> pp x
      Qual x y -> pp x <.> "." <.> pp y

data Name = Name {
  nameFileId :: Int,
  nameFile   :: FilePath,
  nameText   :: Text
}

instance Eq Name where
  x == y = nameFileId x == nameFileId y && nameText x == nameText y

instance Ord Name where
  compare x y = compare (nameFileId x, nameText x) (nameFileId y, nameText y)

instance Show Name where
  show x = show (nameFileId x, nameText x)

data Decl dnm unm = Decl {
  declName  :: dnm,
  declDef   :: Type unm,
  declRange :: SourceRange,
  declDocs  :: [Text]
} deriving Show

data Type nm =
    TExact Value
  | TBuiltIn BuiltIn
  | Type nm :| Type nm
  | TObject (FieldSpec nm)
  | TArray (Type nm)
  | TTuple [Type nm]
  | TNamed nm
  | TLocated SourceRange (Type nm)
    deriving Show

typeRange :: Type nm -> Maybe SourceRange
typeRange ty =
  case ty of
    TLocated r _ -> Just r
    _ -> Nothing

data BuiltIn = TAny | TNumber | TBoolean | TString
  deriving Show

data Value = VNull | VBool Bool | VNumber Scientific | VString Text
  deriving (Show,Eq,Ord)

data FieldSpec nm =
    CaseField Text (Map Value (SourceRange, FieldSpec nm))
  | MatchFields (FieldSpecAnd nm)
  | OrFields (FieldSpec nm) (FieldSpec nm)
    deriving Show

data FieldSpecAnd nm = FieldSpecAnd {
  matchFields   :: Map Text (FieldName, Type nm),
  otherFieldsOk :: !Bool
} deriving Show

andFields :: FieldSpecAnd nm -> FieldSpecAnd nm -> Either [(FieldName,FieldName)] (FieldSpecAnd nm)
andFields fs1 fs2
  | Map.null bad = Right FieldSpecAnd {
      matchFields = Map.union (matchFields fs1) (matchFields fs2),
      otherFieldsOk = otherFieldsOk fs1 || otherFieldsOk fs2
    }
  | otherwise = Left (Map.elems bad)
  where
  bad = Map.intersectionWith (\(x,_) (y,_) -> (x,y)) (matchFields fs1) (matchFields fs2) 
  

noLoc :: Type nm -> Type nm
noLoc t =
  case t of
    TLocated _ t1 -> noLoc t1
    _ -> t

orTypes :: PP nm => Type nm -> Type nm -> Type nm
orTypes t1 t2 =
  case (noLoc t1, noLoc t2) of
    (TObject s1, TObject s2) -> TObject (orFields s1 s2)
    _ -> t1 :| t2


orFields :: PP nm => FieldSpec nm -> FieldSpec nm -> FieldSpec nm
orFields fs1 fs2 =
  case (fs1, fs2) of
    (MatchFields a, MatchFields b) -> matchMatch a b
    (MatchFields a, CaseField f ks) -> caseMatch f ks a
    (CaseField f ks, MatchFields a) -> caseMatch f ks a
    (CaseField f1 ks1, CaseField f2 ks2) -> caseCase f1 ks1 f2 ks2
    _ -> dflt
  where
  dflt = OrFields fs1 fs2

  caseCase f1 ks1 f2 ks2
    | f1 == f2 = CaseField f1 (Map.mergeWithKey (\_ x y -> Just (joinK x y)) id id ks1 ks2)
    | otherwise = dflt

  caseMatch f ks fsS =
    let fs = matchFields fsS
    in
    case Map.lookup f fs of
      Just (fn,ty) | fieldRequired fn, Just vs <- isK ty [] ->
        let k = (fieldRange fn, MatchFields fsS { matchFields = Map.delete f fs })
        in CaseField f $ foldr (\v mp -> Map.insertWith joinK v k mp) ks vs
      _ -> dflt

  isK ty vs =
    case ty of
      TLocated _ r -> isK r vs
      t1 :| t2 -> isK t1 =<< isK t2 vs
      TExact v -> Just (v : vs)
      _ -> Nothing

  joinK (r,a) (_,b) = (r, orFields a b) -- XXX: location?

  matchMatch fs gs =
    let fieldsF = matchFields fs
        fieldsG = matchFields gs
        common = Map.intersectionWith (,) fieldsF fieldsG
        okTag ((f1,t1),(f2,t2))
          | fieldRequired f1, fieldRequired f2,
            Just vs1 <- isK t1 [], Just vs2 <- isK t2 [],
            let nm = fieldName f1
                lhs = (fieldRange f1, MatchFields fs { matchFields = Map.delete nm fieldsF })
                rhs = (fieldRange f2, MatchFields gs { matchFields = Map.delete nm fieldsG })
          = Just (nm, [ (v1, lhs) | v1 <- vs1 ] ++ [ (v2, rhs) | v2 <- vs2 ])
          | otherwise = Nothing
    in case Map.minView (Map.mapMaybe okTag common) of
        Just ((f,ks),_) -> CaseField f (Map.fromListWith joinK ks)
        Nothing -> dflt


data Field nm =
    FieldName :> Type nm
  | OtherFields
  deriving Show

data FieldName = FieldName {
  fieldName     :: Text,
  fieldRequired :: Bool,
  fieldRange    :: SourceRange,
  fieldDoc      :: [Text]
} deriving Show

instance (PP a, PP b) => PP (Decl a b) where
  pp d = "type" <+> pp (declName d) <+> "=" $$ nest 2 (pp (declDef d))

instance PP a => PP (Type a) where
  pp ty =
    case ty of
      TExact v -> pp v
      TBuiltIn t -> pp t

      _ :| _ ->
        let ts = splitOr ty []
            ds = map pp ts
        in
        if all isSmallType ts
          then hsep (intersperse "|" ds)
          else vcat (zipWith (<+>) ("  " : repeat "| ") ds)

      
      TObject fs -> "{" $$ nest 2 (pp fs) $$ "}"

      TArray t ->
        let d = pp t
        in case noLoc t of
             _ :| _ -> parens d <.> "[]"
             _ -> d <.> "[]"
      TTuple xs -> brackets (hsep (punctuate comma (map pp xs)))
      TNamed a -> pp a
      TLocated _ t -> pp t

-- | Is this type considered to be "small" for pretty printing purposes
isSmallType :: Type a -> Bool
isSmallType x =
  case x of
    TExact {} -> True
    TBuiltIn {} -> True
    _ :| _ -> False
    TObject {} -> False
    TArray el -> isSmallType el
    TTuple es -> all isSmallType es
    TNamed {} -> True
    TLocated _ t -> isSmallType t 

-- | Eliminate top-level Ors and TLocated, used for pretty printing.
splitOr :: Type nm -> [Type nm] -> [Type nm]
splitOr xs ys =
      case xs of
        a :| b -> splitOr a (splitOr b ys)
        TLocated _ t -> splitOr t ys
        _ -> xs : ys

-- | Eliminate top-level Ors, looking through defintions.
splitOrWithDefs :: Ord a => Maybe SourceRange -> Map a (Decl a a) -> Type a -> [Type a] -> [Type a]
splitOrWithDefs mbLoc defs xs ys =
      case xs of
        a :| b -> splitOrWithDefs Nothing defs a (splitOrWithDefs Nothing defs b ys)
        TLocated r t -> splitOrWithDefs (Just r) defs t ys
        TNamed x ->
          case Map.lookup x defs of
            Just d -> splitOrWithDefs Nothing defs (declDef d) ys
            Nothing -> ys
        _ -> case mbLoc of
               Nothing -> xs : ys
               Just r  -> TLocated r xs : ys


-- We could expand back to normal fields, but we print the actual
-- type for debugging.
instance PP nm => PP (FieldSpec nm) where
  pp s =
    case s of
      CaseField f ks ->
        ("case" <+> text (show f) <+> "of") $$ nest 2 (vcat (map ppAlt (Map.toList ks)))
        where
        ppAlt (k,(_,v)) = (pp k <+> "->") $$ nest 2 (pp v)
      MatchFields f -> pp f
      OrFields {} ->
        vcat (zipWith (<+>) ("OR" : repeat "| ") (map pp (splitOrs s [])))
        where
        splitOrs xs ys =
          case xs of
            OrFields as bs -> splitOrs as (splitOrs bs ys)
            _ -> xs : ys
        

instance PP nm => PP (FieldSpecAnd nm) where
  pp x
    | otherFieldsOk x = (vcat ds <.> comma) $$ "..."
    | otherwise = vcat ds
    where
    ds = punctuate comma [ pp f <.> ":" <+> pp t | (f,t) <- Map.elems (matchFields x) ]
    

instance PP BuiltIn where
  pp ty =
    case ty of
      TNumber -> "number"
      TBoolean -> "boolean"
      TString -> "string"
      TAny -> "any"

instance PP Value where
  pp val =
    case val of
      VNull -> "null"
      VBool b -> if b then "true" else "false"
      VNumber n -> text (show n) -- XXX: does not show JS format?
      VString t -> text (show t) -- XXX: does not show in JS fromat

instance PP FieldName where
  pp f = if fieldRequired f then nm else nm <.> "?"
    where nm = text (show (fieldName f)) 

instance PP a => PP (Field a) where
  pp f =
    case f of
      x :> t
       | isSmallType t -> pp x <.> ":" <+> pp t
       | otherwise -> pp x <+> ":" $$ nest 2 (pp t)
      OtherFields -> "..."

instance PP Name where
  pp n = pp (nameText n)