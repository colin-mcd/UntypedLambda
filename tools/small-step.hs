module Main where
import Data.List (intercalate)
import SmallStep
import Helpers

main = readTerms (\ t -> intercalate "\n" (map show (reductions t)))
