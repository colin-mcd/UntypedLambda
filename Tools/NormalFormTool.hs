module Main where
import Helpers
import Reduce

main :: IO ()
main = readTerms (show . normalForm)
