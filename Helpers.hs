module Helpers where
import System.IO (hPutStrLn, stderr, hFlush)
import System.Exit (exitFailure)
import Struct
import Subst
import Data.Map (Map, null, fromList)

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

pairs :: [a] -> [(a, a)]
pairs (a1 : a2 : as) = (a1, a2) : pairs as
pairs _ = []

-- Break off into a new sublist whenever the predicate is true
-- (Squashes all empty sublists)
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f as = h [] as where
  h [] [] = []
  h acc [] = [reverse acc]
  h [] (a : as) = h [a] as
  h acc (a : as) = if f a then reverse acc : h [a] as else h (a : acc) as

guardFVs :: (Term -> String) -> (Term -> String)
guardFVs f t =
  if Data.Map.null (freeVars t) then f t else "Expression contains free variables"

progDefs :: Program -> Map String Term
progDefs (Program ds) = fromList [(x, t) | TermDef x t <- ds]

guardIO :: IO (Either String a) -> IO a
guardIO i = i >>= either (\ e -> hPutStrLn stderr e >> hFlush stderr >> exitFailure) return
