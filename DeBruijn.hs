module DeBruijin where

-- De Bruijn-indexed term
data DBTm =
    DBVar Int
  | DBApp DBTm DBTm
  | DBLam DBTm

toDBTm' :: [String] -> Term -> DBTm
toDBTm' xs (Var x) = maybe (error "This shouldn't happen") DBVar (elemIndex x xs)
toDBTm' xs (App t u) = DBApp (toDBTm' xs t) (toDBTm' xs u)
toDBTm' xs (Lam x t) = DBLam (toDBTm' (x : xs) t)

