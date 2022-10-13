module Main where
import Struct
import Parse
import Lex
import Bohm
import Reduce

main :: IO ()
main =
  getLine >>= \ s1 ->
  getLine >>= \ s2 ->
  let p1 = lexStr (parens s1) >>= parseOut parseTerm1
      p2 = lexStr (parens s2) >>= parseOut parseTerm2
      c = p1 >>= \ t1 ->
          p2 >>= \ t2 ->
          maybe (Left "Terms are inseparable") Right
            (makeContradiction (normalForm t1) (normalForm t2))
  in
{-    either (\ e -> putStrLn ("In first term: " ++ e))
           (\ t1 -> either (\ e -> putStrLn ("In second term: " ++ e))
                           (\ t2 -> putStrLn (show (normalForm t1)) >>
                                    putStrLn (show (normalForm t2))) p2) p1 >>
-}
    putStrLn (either id show c)
