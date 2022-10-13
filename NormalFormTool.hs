module Main where
import Struct
import Lex
import Parse
import Reduce

main :: IO ()
main =
  lines <$> getContents >>= \ ls ->
  let ps = [lexStr (parens s) >>= parseOut parseTerm1 | s <- ls]
      ips = [either (\ e -> Left ("In line " ++ show i ++ ": " ++ e)) Right p
            | (i, p) <- zip [0..] ps]
  in
    either (\ e _ -> putStrLn e) (\ ps c -> c ps) (sequence ips) $ \ ts ->
    mapM (\ t -> putStrLn (show (normalForm t))) ts >>
    return ()
