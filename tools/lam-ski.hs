module Main where
import Helpers
import SKI

main = readTerms (guardFVs $ show . skiToIota . convert)
