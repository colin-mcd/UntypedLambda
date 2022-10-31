module Main where
import Parse
import Lex
--import Struct
--import System.Environment

process_file :: String -> Either String String
process_file text =
  lexFile text >>= \ ts ->
  parseFile ts >>= \ ps ->
  return (show ps)

--main :: IO ()
main =
  getContents >>=
  either putStrLn putStrLn . process_file
