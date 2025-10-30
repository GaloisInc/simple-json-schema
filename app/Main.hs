module Main where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map qualified as Map
import Control.Exception
import Control.Monad(when)
import SimpleGetOpt
import System.Exit
import System.IO
import AlexTools
import ParserUtils(ParseError(..), parseFromFile)
import Resolve
import JSON.Parser(value)
import PP
import Validate


main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    when (optShowHelp opts)
      do dumpUsage options
         exitSuccess
    schemaFile <-
      case optSchema opts of
        Nothing -> reportUsageError options ["No schema was provided."]
        Just s  -> pure s

    (errs, mbRoot, ds) <- parseSpecAndDeps schemaFile (optEntry opts)
      `catch` \(_ :: IOException) ->
        do hPutStrLn stderr ("Cannot read file " ++ show schemaFile)
           exitFailure
    when (optDebugPP opts) (print (vcat (map pp (Map.elems ds))))
    case (errs, mbRoot) of
      ([], Just e) ->
        case optValidate opts of
          [] -> putStrLn "Schema file is OK"
          fs -> mapM_ validateFile fs
            where
            showErrors es =
              do
                putStrLn "  [ERRORS]"
                hPrint stderr (nest 2 (vcat es))

            validateFile f =
              do
                putStr f
                hFlush stdout
                js <- parseFromFile value f
                case validateDecl ds e js of
                  Match -> putStrLn "  [OK]"
                  PartialMatch _ es  -> showErrors (map pp es)
                  Mismatch es  -> showErrors (map pp es)
              `catches` [
                Handler \(ParseError p) -> showErrors [ text (prettySourcePosLong p) <.> ": parse error" ],
                Handler \(_ :: IOException) -> showErrors [ "Cannot read file" <+> text (show f) ]
              ]

                 
      _  ->
        do
          mapM_ (hPrint stderr . pp) errs
          case mbRoot of
            Nothing -> hPutStrLn stderr "Undefined entry point"
            Just _ -> pure ()
          exitFailure
      
  `catches` [
    Handler \(ParseError p) ->
      do
        hPutStrLn stderr (prettySourcePosLong p ++ ": parse error")
        exitFailure,
    Handler \(BadImport srcRange file _err) ->
      do
        hPutStrLn stderr (prettySourcePosLong (sourceFrom srcRange) ++ ": Cannot read from file " ++ show file)
        exitFailure
  ]

data Options = Options {
  optValidate :: [FilePath],
  optShowHelp :: Bool,
  optDebugPP  :: Bool,
  optSchema   :: Maybe FilePath,
  optEntry    :: Maybe Text
}

defaultOptions :: Options
defaultOptions = Options {
  optValidate = [],
  optShowHelp = False,
  optDebugPP  = False,
  optSchema   = Nothing,
  optEntry    = Nothing
}

options :: OptSpec Options
options = optSpec {
  progDescription = [ "A tool for specifying and validating JSON schemas." ],
  progOptions = [
    Option [] ["validate"]
    "Validate the given JSON file"
    $ ReqArg "FILE" \s o -> Right o { optValidate = s : optValidate o },

    Option [] ["entry"]
    "Specify the root declaration of the schema."
    $ ReqArg "IDENT" \s o ->
      case optEntry o of
        Nothing -> Right o { optEntry = Just (Text.pack s) }
        Just _  -> Left "Multiple `entry` flags",

    Option [] ["dbg-pp"]
    "Pretty print the parsed schema."
    $ NoArg \o -> Right o { optDebugPP = True },
      
    Option [] ["help"]
    "Show this help"
    $ NoArg \o -> Right o { optShowHelp = True }
  ],
  progParamDocs =
    [ ("FILE", "Schema to use for validation") ],

  progParams = \s o ->
    case optSchema o of
      Nothing -> Right o { optSchema = Just s }
      Just _  -> Left "The tool expects a single schema file."
}


