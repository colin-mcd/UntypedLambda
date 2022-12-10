module Main where
import System.Environment (getArgs, getProgName)
import Data.List (intercalate)
import Reduce
import Parse
import Bohm
import Helpers
import Struct

usage :: String -> String
usage progname = intercalate "\n" [
  "Usage:",
  ("  " ++ progname ++ " [--show] [--with FILES...]"),
  "Reads two terms and computes a discriminator function",
  "that returns true (\\t. \\f. t) when given the first",
  "and false (\\t. \\f. f) when given the second.",
  "With --show, print discriminator applied to each term."
  ]

readOptions :: IO (Either String (Bool, [String]))
readOptions =
  getArgs >>= \ as ->
  case as of
    ("--show" : "--with" : as) -> return (Right (True, as))
    ("--show" : []) -> return (Right (True, []))
    ("--with" : as) -> return (Right (False, as))
    [] -> return (Right (False, []))
    _ -> getProgName >>= return . Left . usage

main =
  setBuffering >>
  guardIO readOptions >>= \ (sh, fs) ->
  parseFiles fs >>= \ p ->
  let ctxt = progDefs p in
    readTwoTerms $ \ t u ->
      maybe
        (Left $ "Terms " ++ show t ++ " and " ++ show u ++ " are inseparable")
        (\ d -> Right $ if sh then (show (App d t) ++ "\n" ++ show (App d u)) else show d)
        (makeContradiction (reduce ctxt NormOrder t) (reduce ctxt NormOrder u))
