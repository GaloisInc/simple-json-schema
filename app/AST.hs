module AST where

import Data.Text
import AlexTools(SourceRange)
import PP

data Module = Module {
  moduleImports :: [Import],
  moduleDecls   :: [Decl Text QName]
} deriving Show

data Import = Import {
  importSpec  :: ImportSpec ,
  importAs    :: Maybe Text,
  importFile  :: Text,
  importId    :: Int, -- ^ File Id, filled in by Resolve
  importRange :: SourceRange
} deriving Show

data ImportSpec =
    ImportAll
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
  declRange :: SourceRange
} deriving Show

data Type nm =
    TExact Value
  | Type nm :| Type nm
  | TNumber | TBoolean | TString
  | TObject [Field nm]
  | TArray (Type nm)
  | TTuple [Type nm]
  | TNamed nm
  | TLocated SourceRange (Type nm)
  | TAny
    deriving Show

typeRange :: Type nm -> Maybe SourceRange
typeRange ty =
  case ty of
    TLocated r _ -> Just r
    _ -> Nothing

data Value = VNull | VBool Bool | VInt Text | VString Text
  deriving Show

data Field nm = FieldName :> Type nm
  deriving Show

data FieldName = FieldName {
  fieldName     :: Text,
  fieldRequired :: Bool,
  fieldRange    :: SourceRange
} deriving Show
