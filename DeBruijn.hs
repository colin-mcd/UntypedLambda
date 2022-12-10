module DeBruijin where
import Data.List (elemIndex)
import Struct

-- De Bruijn-indexed term
data DBTm =
    DBVar Int
  | DBApp DBTm DBTm
  | DBLam DBTm

toDBTm' :: [String] -> Term -> DBTm
toDBTm' xs (Var x) = maybe (error "This shouldn't happen") DBVar (elemIndex x xs)
toDBTm' xs (App t u) = DBApp (toDBTm' xs t) (toDBTm' xs u)
toDBTm' xs (Lam x t) = DBLam (toDBTm' (x : xs) t)

lamToDB :: Term -> DBTm
lamToDB = toDBTm' []

dBToLam :: DBTm -> Term
dBToLam = h 0 where
  h n (DBVar x) = Var ('x' : show (n - x))
  h n (DBApp t u) = App (h n t) (h n u)
  h n (DBLam t) = Lam ('x' : show n) (h (succ n) t)



------------------------------
-- Printing
------------------------------

-- Does printing term t need parens, given history s?
needParensDB ShowNone _ = False
needParensDB s (DBVar _) = False
needParensDB ShowLam  (DBApp _ _) = False
needParensDB ShowAppL (DBApp _ _) = False
needParensDB ShowAppR (DBApp _ _) = True
needParensDB ShowLam  (DBLam _) = False
needParensDB ShowAppL (DBLam _) = True
needParensDB ShowAppR (DBLam _) = True -- abc (\x. x)

showDBTermh (DBVar x) = show x
showDBTermh (DBApp t u) = showDBTerm ShowAppL t ++ " " ++ showDBTerm ShowAppR u
showDBTermh (DBLam t) = "λ" ++ showDBTerm ShowLam t

showDBTerm s t = doIf (needParensDB s t) parens (showDBTermh t)

instance Show DBTm where
  show = showDBTerm ShowNone
