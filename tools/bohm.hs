module Main where
import System.Environment (getArgs, getProgName)
import Data.List (intercalate)
import Reduce
import Parse
import Bohm
import Helpers

usage :: String -> String
usage progname = intercalate "\n" [
  "Usage:",
  ("  " ++ progname ++ " [--with FILES...]"),
  "Reads two terms and computes a discriminator function",
  "that returns true (\\t. \\f. t) when given the first",
  "and false (\\t. \\f. f) when given the second."
  ]

readOptions :: IO (Either String [String])
readOptions =
  getArgs >>= \ as ->
  case as of
    [] -> return (Right [])
    ("--with" : as) -> return (Right as)
    _ -> getProgName >>= return . Left . usage

main =
  guardIO readOptions >>= \ fs ->
  parseFiles fs >>= \ p ->
  let ctxt = progDefs p in
    readTwoTerms $ \ t u ->
      maybe "Terms are inseparable" show
        (makeContradiction (reduce ctxt NormOrder t) (reduce ctxt NormOrder u))
