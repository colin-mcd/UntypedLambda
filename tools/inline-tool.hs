module Main where
import Inline
import Parse
import Lex

main =
  getContents >>= \ s ->
  let e = lexStr s >>= parseFile >>= \ f -> Right (inline f) in
    either putStrLn (putStrLn . show) (e)
