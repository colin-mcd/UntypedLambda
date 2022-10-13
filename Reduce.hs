module Reduce where
import Struct
import Subst

-- Don't reduce under lambdas
reduce :: Term -> Term
reduce (Lam x t) = Lam x t
reduce (Var x) = Var x
reduce (App t u) = case App (reduce t) u of
  App (Lam x t) u -> reduce ((x |-> u) t)
  tu -> tu

-- Reduce to normal form
normalForm :: Term -> Term
normalForm (Lam x t) = Lam x (normalForm t)
normalForm (Var x) = Var x
normalForm (App t u) = case App (normalForm t) u of
  App (Lam x t) u -> normalForm ((x |-> u) t)
  App t u -> App t (normalForm u)


