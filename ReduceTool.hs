module Main where
import Struct
import Lex
import Parse
import Reduce

main :: IO ()
main =
  getLine >>= \ s ->
  let p = lexStr s >>= parseOut parseTerm1 >>= \ t -> Right (reduce t) in
    putStrLn (either id show p)
