module Resolve (parseSpecAndDeps, ResolveError(..)) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.List(transpose)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Control.Monad(when)
import System.Directory(canonicalizePath)
import System.FilePath(takeExtension, addExtension, splitPath, joinPath)
import AlexTools
import PP
import Parser
import AST

ext :: String
ext = ".schema"


data ResolveError =
    MultipleDefinitions Text [ SourceRange ]
  | UndefinedName SourceRange QName
  | AmbiguousName SourceRange QName [ Name ]

instance PP ResolveError where
  pp err =
    case err of
      MultipleDefinitions x rs ->
        case rs of
          [] -> "BUG: Missing multiple definitions"
          d : ds ->
            msg d ("Multiple definitions for" <+> pp x <.> ":")
              [ "See also:" <+> text (prettySourcePos (sourceFrom o)) | o <- ds ]

      UndefinedName r q ->
        msg r ("Undefined name" <+> pp q) []

      AmbiguousName r q xs ->
        msg r (pp q <+> "may refer to definitions in:")
              (map text (dropCommonDirs (map nameFile xs)))
    where
    msg r x xs =
      vcat (
        (text (prettySourcePosLong (sourceFrom r)) <.> ":" <+> x) :
        map (nest 2) xs)

    dropCommonDirs =
      map joinPath . transpose . dropWhile allSame . transpose . map splitPath
  
    allSame xs =
      case xs of
        [] -> True
        x : ys -> all (== x) ys


-- | Parse and resolve declarations from a bunch of files.
-- Throws `ParseError`
parseSpecAndDeps :: FilePath -> IO ([ResolveError], Map Name (Decl Name Name))
parseSpecAndDeps file =
  do
    c <- canonicalizePath file
    m <- parseSpecFromFile file
    let s = State { loadedSpec = mempty, nextId = 1 }
        done = Set.singleton c
    (is, s1) <- parseImports done [] s (moduleImports m)
    let m1 = m { moduleImports = is }
        ps = ParsedSpec {
               psId = 0,
               psModule = m1,
               psFile = c
             }
    let parsed = Map.fromList [ (psId p, p) | p <- ps : Map.elems (loadedSpec s1) ]
    pure (Map.unions <$> mapM (resolveModule parsed) (Map.elems parsed))


resolveModule :: Map Int ParsedSpec -> ParsedSpec -> ([ResolveError], Map Name (Decl Name Name))
resolveModule ms m =
  (repeats ++ errs, Map.fromList [ (declName d, d) | d <- resolved ])
  where
  (errs, resolved) = mapM (resolveDecl (psId m) (psFile m) env) (moduleDecls mo)
  mo  = psModule m
  env = Map.unionsWith (++) (localEnv : map (envFromImport ms) (moduleImports mo))
  repeats =
    map ambigError $
    Map.elems $
    Map.filter isAmbig $
    Map.fromListWith (++) [ (declName d, [d]) | d <- moduleDecls mo ]

  ambigError ds =
    case ds of
      [] -> error "ambigError: []"
      d : _ -> MultipleDefinitions (declName d) [ declRange a | a <- ds ]

  isAmbig xs =
    case xs of
      [_] -> False
      _   -> True
  localEnv =
    Map.fromList [ (Unqual (declName d), [
                              Name {
                                nameFileId = psId m,
                                nameFile = psFile m,
                                nameText = declName d
                              }
                              ]) | d <- moduleDecls mo ]

  


resolveDecl :: Int -> FilePath -> Map QName [Name] -> Decl Text QName -> ([ResolveError], Decl Name Name)
resolveDecl i n mp d =
  do
    t1 <- resolveType mp (declRange d) (declDef d)
    pure d {
      declName = Name { nameFileId = i, nameFile = n, nameText = declName d },
      declDef  = t1
    }

resolveType :: Map QName [Name] -> SourceRange -> Type QName -> ([ResolveError], Type Name)
resolveType mp r t =
  case t of
    TExact v -> pure (TExact v)
    t1 :| t2 -> (:|) <$> resolveType mp r t1 <*> resolveType mp r t2
    TNumber -> pure TNumber
    TBoolean -> pure TBoolean
    TString -> pure TString
    TObject fs -> TObject <$> mapM resolveField fs
    TArray elT -> TArray <$> resolveType mp r elT
    TTuple ts -> TTuple <$> mapM (resolveType mp r) ts
    TAny -> pure TAny
    TNamed n ->
      case Map.lookup n mp of
        Just [x] -> pure (TNamed x) 
        Just xs@(x1 : _) -> ([AmbiguousName r n xs], TNamed x1)
        _                -> ([UndefinedName r n], TAny)
    TLocated r1 t1 -> TLocated r1 <$> resolveType mp r1 t1
  where
  resolveField (f :> ft) =
    do t1 <- resolveType mp r ft
       pure (f :> t1)

envFromImport :: Map Int ParsedSpec -> Import -> Map QName [Name]
envFromImport ms imp =
  case Map.lookup i ms of
    Nothing -> mempty
    Just m -> Map.fromList [ name (psFile m) (declName d) | d <- moduleDecls (psModule m) ]
  where
  i        = importId imp
  qual     = maybe Unqual Qual (importAs imp)
  name f x = (qual x, [Name { nameFile = f, nameText = x, nameFileId = i }])


data ParsedSpec = ParsedSpec {
  psId     :: Int,
  psModule :: Module,
  psFile   :: FilePath
}

data State = State {
  loadedSpec :: Map FilePath ParsedSpec,
  nextId     :: Int
}


parseImports :: Set FilePath -> [Import] -> State -> [Import] -> IO ([Import], State)
parseImports done doneIs s imps =
  case imps of
    [] -> pure (reverse doneIs, s)
    imp : is ->
      do (i,s1) <- parseImport done s imp
         parseImports done (i : doneIs) s1 is
          
parseImport :: Set FilePath -> State -> Import -> IO (Import, State)
parseImport done s imp =
  do
    let f' = Text.unpack (importFile imp)
        f = if takeExtension f' == ext then f' else addExtension f' ext
    c <- canonicalizePath f
    when (c `Set.member` done)
      (fail (show (importRange imp) ++ ": Recusive dependency."))
    (n,s1) <-
      case Map.lookup c (loadedSpec s) of
        Just l -> pure (psId l, s)
        Nothing ->
          do
              m <- parseSpecFromFile f
              let done1 = Set.insert c done
              let i  = nextId s
                  s1 = s { nextId = i + 1 }
              (is1,s2) <- parseImports done1 [] s1 (moduleImports m)
              let p  = ParsedSpec {
                        psId = i,
                        psModule = m { moduleImports = is1 },
                        psFile = c
                      }
                  s3 = s2 { loadedSpec = Map.insert c p (loadedSpec s2) }
              pure (i, s3)
    pure (imp { importId = n }, s1)


