module Main where

import Data.Map qualified as Map
import Control.Exception
import System.Environment
import AlexTools
import ParserUtils(ParseError(..))
import Resolve
import PP


main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [f] ->
        do
           (errs, ds) <- parseSpecAndDeps f
           case errs of
              [] -> print (Map.elems ds)
              _  -> mapM_ (print . pp) errs
      _ ->  putStrLn "Error: Need a single file"
  `catches` [
     Handler \(ParseError p) -> putStrLn (prettySourcePosLong p ++ ": parse error")
  ]
