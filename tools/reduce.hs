module Main where
import System.Environment (getArgs, getProgName)
import Helpers
import Reduce
import Struct

data ReductionStrategy =
    CallByName
  | CallByValue
  | NormOrder
  | ApplOrder

defaultStrategy = CallByValue

usage :: IO ()
usage =
  getProgName >>= \ p ->
  putStrLn (unlines [
    "Usage:",
    (p ++ " [flag]"),
    "  --cbn  call-by-name reduction",
    "  --cbv  call-by-value",
    "  --norm normal order",
    "  --appl applicative order"
  ])

reduceReadArgs :: [String] -> Maybe ReductionStrategy
reduceReadArgs ("--cbn" : as) = Just CallByName
reduceReadArgs ("--cbv" : as) = Just CallByValue
reduceReadArgs ("--norm" : as) = Just NormOrder
reduceReadArgs ("--appl" : as) = Just ApplOrder
reduceReadArgs (_ : as) = Nothing
reduceReadArgs [] = Just defaultStrategy

reduceBy :: ReductionStrategy -> Term -> Term
reduceBy CallByName = reduceCBN
reduceBy CallByValue = reduceCBV
reduceBy NormOrder = reduceNorm
reduceBy ApplOrder = reduceAppl

main :: IO ()
main =
  getArgs >>= \ as ->
  maybe usage (\ strat -> readTerms (show . reduceBy strat)) (reduceReadArgs as)
  --readTerms (show . betaReduce)
