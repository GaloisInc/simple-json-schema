module Resolve (parseSpecAndDeps, ResolveError(..), BadImport(..)) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.List(transpose)
import Data.Maybe(listToMaybe)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Graph.SCC(stronglyConnComp)
import Data.Graph(SCC(..))
import Control.Monad(when,mapAndUnzipM)
import Control.Exception(Exception, throwIO, catch, IOException)
import System.Directory(canonicalizePath)
import System.FilePath(takeExtension, addExtension, splitPath, joinPath, isRelative, takeDirectory, (</>))
import AlexTools
import PP
import Parser(spec)
import ParserUtils
import AST

ext :: String
ext = ".ts"

data ResolveError =
    MultipleDefinitions Text [ SourceRange ]
  | UndefinedName SourceRange QName
  | AmbiguousName SourceRange QName [ Name ]
  | RecursiveDeclarations [(SourceRange,Name)]

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

      RecursiveDeclarations ds ->
        case ds of
          [] -> "Recusive declarations"
          [(r,x)] -> msg r (pp x <+> "depends on itself") []
          (r,_) : _ -> msg r "Mutually recursive definitions:" (map sh ds)
            where
            p = sourceFrom r
            sh (r1,y) =
              let p1 = sourceFrom r1
                  loc = if sourceFile p == sourceFile p1
                          then prettySourcePos p1 else prettySourcePosLong p1
              in text loc <.> ":" <+> pp y
            
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
parseSpecAndDeps ::
  FilePath {- ^ File containing spec -} ->
  Maybe Text {- ^ Optional root name -} ->
  IO ([ResolveError], Maybe Name, Map Name (Decl Name Name))
  -- ^ (errors, root name---if we could find it, declarations)
parseSpecAndDeps file mbRoot =
  do
    c <- canonicalizePath file
    m <- parseFromFile spec file
    let s = State { loadedSpec = mempty, nextId = 1 }
        done = Set.singleton c
    (is, s1) <- parseImports c done [] s (moduleImports m)
    let m1 = m { moduleImports = is }
        ps = ParsedSpec {
               psId = 0,
               psModule = m1,
               psFile = c
             }
    let parsed = Map.fromList [ (psId p, p) | p <- ps : Map.elems (loadedSpec s1) ]
        (errs, (roots, decls)) = mapAndUnzipM (resolveModule parsed) (Map.elems parsed)
    let root =
          case mbRoot of
            Nothing ->
              case roots of
                [] -> Nothing
                mb : _ -> mb
            Just x
              | any ((== x) . declName) (moduleDecls m1) ->
                Just Name { nameFileId = 0, nameFile = c, nameText = x }
              | otherwise -> Nothing
    let mp = Map.unions decls
    let allErrs = checkRecursive mp ++ errs
    pure (allErrs, root, mp)

checkRecursive :: Map Name (Decl Name Name) -> [ResolveError]
checkRecursive mp = [ RecursiveDeclarations ns | CyclicSCC ns <- recs ]
  where
  recs = stronglyConnComp (map node (Map.elems mp))
  node d =
    let deps = directDeps (declDef d)
    in ((declRange d, declName d), declName d, Set.toList deps)

-- | Types that we depend on that are not under a constructor
directDeps :: Type Name -> Set Name
directDeps ty =
  case ty of
    TExact {} -> mempty
    TBuiltIn {} -> mempty
    TObject {} -> mempty
    TArray {} -> mempty
    TTuple {} -> mempty
    TNamed x -> Set.singleton x
    TLocated _ t -> directDeps t
    x :| y -> Set.union (directDeps x) (directDeps y)




resolveModule :: Map Int ParsedSpec -> ParsedSpec -> ([ResolveError], (Maybe Name, Map Name (Decl Name Name)))
resolveModule ms m =
  (repeats ++ errs, (root, Map.fromList [ (declName d, d) | d <- resolved ]))
  where
  root = declName <$> listToMaybe resolved
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
    TBuiltIn b -> pure (TBuiltIn b)
    t1 :| t2 -> (:|) <$> resolveType mp r t1 <*> resolveType mp r t2
    TObject fs -> TObject <$> mapM resolveField fs
    TArray elT -> TArray <$> resolveType mp r elT
    TTuple ts -> TTuple <$> mapM (resolveType mp r) ts
    TNamed n ->
      case Map.lookup n mp of
        Just [x] -> pure (TNamed x) 
        Just xs@(x1 : _) -> ([AmbiguousName r n xs], TNamed x1)
        _                -> ([UndefinedName r n], TBuiltIn TAny)
    TLocated r1 t1 -> TLocated r1 <$> resolveType mp r1 t1
  where
  resolveField fi =
    case fi of
      f :> ft ->
        do t1 <- resolveType mp r ft
           pure (f :> t1)
      OtherFields -> pure OtherFields

envFromImport :: Map Int ParsedSpec -> Import -> Map QName [Name]
envFromImport ms imp =
  case Map.lookup i ms of
    Nothing -> mempty
    Just m ->
      Map.fromList [ name (psFile m) (declName d)
                   | d <- moduleDecls (psModule m), consider (declName d) ]
  where
  i        = importId imp
  (consider,qual) =
    case importSpec imp of
      ImportAll q -> (const True, Qual q)
      ImportList xs -> ((`elem` xs), Unqual)
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

data BadImport = BadImport SourceRange FilePath IOException deriving Show
instance Exception BadImport

parseImports :: FilePath -> Set FilePath -> [Import] -> State -> [Import] -> IO ([Import], State)
parseImports file done doneIs s imps =
  case imps of
    [] -> pure (reverse doneIs, s)
    imp : is ->
      do (i,s1) <- parseImport file done s imp
         parseImports file done (i : doneIs) s1 is
          
parseImport :: FilePath -> Set FilePath -> State -> Import -> IO (Import, State)
parseImport file done s imp =
  do
    let f1 = Text.unpack (importFile imp)
        f2 = if takeExtension f1 == ext then f1 else addExtension f1 ext
        f  = if isRelative f2 then takeDirectory file </> f2 else f2
    c <- canonicalizePath f
    when (c `Set.member` done)
      (fail (show (importRange imp) ++ ": Recusive dependency."))
    (n,s1) <-
      case Map.lookup c (loadedSpec s) of
        Just l -> pure (psId l, s)
        Nothing ->
          do
            m <- parseFromFile spec f `catch` \e ->
                  throwIO (BadImport (importRange imp) f e)
            let done1 = Set.insert c done
            let i  = nextId s
                s1 = s { nextId = i + 1 }
            (is1,s2) <- parseImports c done1 [] s1 (moduleImports m)
            let p  = ParsedSpec {
                      psId = i,
                      psModule = m { moduleImports = is1 },
                      psFile = c
                    }
                s3 = s2 { loadedSpec = Map.insert c p (loadedSpec s2) }
            pure (i, s3)
    pure (imp { importId = n }, s1)


