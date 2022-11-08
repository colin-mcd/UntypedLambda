module Helpers where
import Struct
import Parse
import Lex
import Subst
import Data.Map (null)

readTerms :: (Term -> String) -> IO ()
readTerms f =
  lines <$> getContents >>= \ ls ->
  foldr (\ s rest i -> putStrLn (either id f (lexStrL s i >>= parseOut parseTerm1)) >> rest (succ i)) (\ _ -> return ()) ls 1

pairs :: [a] -> [(a, a)]
pairs (a1 : a2 : as) = (a1, a2) : pairs as
pairs _ = []

readTwoTerms :: (Term -> Term -> String) -> IO ()
readTwoTerms f =
  map (\ (st, su) -> st >>= \ t -> su >>= \ u -> return (t, u)) <$> pairs <$>
    map (\ (i, s) -> lexStrL s i >>= parseOut parseTerm1) <$>
    zip [1..] <$> lines <$> getContents >>=
  foldr (\ ts rest -> putStrLn (either id (uncurry f) ts) >> rest) (return ())

guardFVs :: (Term -> String) -> (Term -> String)
guardFVs f t =
  if Data.Map.null (freeVars t) then f t else "Expression contains free variables"
