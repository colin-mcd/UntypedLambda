module Main where
import Helpers
import Bohm

main =
  readTwoTerms $ \ t u ->
    maybe "Terms are inseparable" show
      (makeContradiction t u)
