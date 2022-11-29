module Main where
import System.Environment (getArgs, getProgName)
import Data.List (intercalate)
import Helpers
import Reduce
import Parse (readTerms, parseFiles)

-- p is program name
usageStr p = unlines [
  "Usage:",
  ("  " ++ p ++ " [flags]"),
  "Reduction strategy flags (default --norm):",
  "  --cbn          call-by-name reduction",
  "  --cbv          call-by-value",
  "  --norm         normal order",
  "  --appl         applicative order",
  "Verbosity flags (default --big):",
  "  --small        small-step reduction (show all intermediate steps)",
  "  --big          big-step reduction (show only final form) [default]",
  "Global context flags:",
  "  --with FILE    use definitions from FILE"
  ]

usage :: IO String
usage = usageStr <$> getProgName

data StepVerbosity = BigStep | SmallStep

data ReduceOpts = ReduceOpts {
  strategy :: ReductionStrategy,
  verbosity :: StepVerbosity,
  withFile :: Maybe String
}

defaultOpts = ReduceOpts {
  strategy = CallByName,
  verbosity = BigStep,
  withFile = Nothing
}

reduceOpts :: IO (Either String ReduceOpts)
reduceOpts = getArgs >>= \ as -> maybe (Left <$> usage) (return . Right) (h as defaultOpts)
  where
    h :: [String] -> ReduceOpts -> Maybe ReduceOpts
    h ("--cbn" : as) o = h as (o {strategy = CallByName})
    h ("--cbv" : as) o = h as (o {strategy = CallByValue})
    h ("--norm" : as) o = h as (o {strategy = NormOrder})
    h ("--appl" : as) o = h as (o {strategy = ApplOrder})
    h ("--small" : as) o = h as (o {verbosity = SmallStep})
    h ("--big" : as) o = h as (o {verbosity = BigStep})
    h ("--with" : fn : as) o = h as (o {withFile = Just fn})
    h (_ : as) o = Nothing
    h [] o = Just o

main :: IO ()
main =
  guardIO reduceOpts >>= \ opts ->
  parseFiles (maybe [] (\ x -> [x]) (withFile opts)) >>= \ p ->
  let g = progDefs p
      s = strategy opts
      v = verbosity opts
  in
    readTerms $ \ t ->
      case v of
        BigStep -> show (reduce g s t)
        SmallStep -> intercalate "\n" (show <$> steps g s t)
