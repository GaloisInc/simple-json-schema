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
  | TObject [Field nm]
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
  deriving Show

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

      
      TObject fs
        | all smallField fs -> "{" <+> hsep ds <+> "}"
        | otherwise -> "{" $$ nest 2 (vcat ds) $$ "}"
        where
        ds = punctuate comma (map pp fs)
        smallField f =
          case f of
            OtherFields -> True
            _ :> t -> isSmallType t
      TArray t ->
        let d = pp t
        in case t of
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