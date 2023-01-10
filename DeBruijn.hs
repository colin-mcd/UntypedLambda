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

-- t[m->u]
dbSubst :: DBTm -> Int -> DBTm -> DBTm
dbSubst t@(DBVar n) m u
  | n == m = u
  | otherwise = t
dbSubst t@(DBApp t1 t2) m u =
  DBApp (dbSubst t1 m u) (dbSubst t2 m u)
dbSubst t@(DBLam t') m u =
  DBLam (dbSubst t' (succ m) (incFVs u 0))
  where
    -- Increments all (free) variables in a term greater than k
    incFVs (DBVar n) k
      | n <  k = n
      | n >= k = succ n
    incFVs (DBApp t1 t2) k = DBApp (incFVs t1) (incFVs t2)
    incFVs (DBLam t) k = DBLam (incFVs t (succ k))

-- Head-normal form: stop reducing once you reach a series of λs with var body head
-- e.g. λλ0, but not λλ(λ0)1
dbHNF :: DBTm -> DBTm
dbHNF (DBVar x) = DBVar x
dbHNF (DBApp t u) =
  case dbCBN t of
    DBLam t' -> dbHNF (dbSubst t' 0 u)
    t' -> DBApp t' u
dbHNF (DBLam t) =
  DBLam (dbHNF t)

-- Call-by-name: stop reducing once you reach a λ
dbCBN :: DBTm -> DBTm
dbCBN (DBVar x) = DBVar x
dbCBN (DBApp t u) =
  case dbCBN t of
    DBLam t' -> dbCBN (dbSubst t' 0 u)
    t' -> DBApp t' u
dbCBN (DBLam t) = DBLam t



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
