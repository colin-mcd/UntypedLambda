module Main where
import Helpers
import Bohm
import Reduce

main =
  readTwoTerms $ \ t u ->
    maybe "Terms are inseparable" show
      (makeContradiction (normalForm t) (normalForm u))
