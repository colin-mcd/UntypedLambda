module Helpers where
import Struct
import Parse
import Lex
import Subst
import Data.Map (null)

parseTerm :: String -> Either String Term
parseTerm s = lexStr s >>= parseOut parseTerm1

-- let x = tm1 in tm2 => (\x. tm2) tm1
letm :: String -> Term -> Term -> Term
letm x tm1 tm2 = App (Lam x tm2) tm1

apps :: Term -> [Term] -> Term
apps t [] = t
apps t (t' : ts) = apps (App t t') ts

unapps :: Term -> (Term, [Term])
unapps (App t u) = let (v, ws) = unapps t in (v, u : ws)
unapps t = (t, [])

lams :: [String] -> Term -> Term
lams [] t = t
lams (x : xs) t = Lam x (lams xs t)

unlams :: Term -> ([String], Term)
unlams (Lam x t) = let (xs, u) = unlams t in (x : xs, u)
unlams t = ([], t)

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
