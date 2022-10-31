module Reduce where
import Struct
import Fresh
import Subst

-- Don't reduce under lambdas
betaReduce :: Term -> Term
betaReduce (Lam x t) = Lam x t
betaReduce (Var x) = Var x
betaReduce (App t u) = case App (betaReduce t) u of
  App (Lam x t) u -> betaReduce ((x |-> u) t)
  tu -> tu

etaReduce :: Term -> Term
etaReduce (Var x) = Var x
etaReduce (App t u) = App (etaReduce t) (etaReduce u)
etaReduce (Lam x t) =
  case etaReduce t of
    App t' (Var x') ->
      if x == x' && not (x `inFV` t') then
        t'
      else
        App t' (Var x')
    t' -> t'

etaExpand :: Term -> Term
etaExpand t =
  let x = fresh "x" (freeVars t) in
    Lam x (App t (Var x))

-- Reduce to normal form, normal order
normalForm :: Term -> Term
normalForm (Lam x t) = Lam x (normalForm t)
normalForm (Var x) = Var x
normalForm (App t u) = case App (normalForm t) u of
  App (Lam x t) u -> normalForm ((x |-> u) t)
  App t u -> App t (normalForm u)
  _ -> error "shouldn't happen"

--alphaConv :: Term -> Term -> Bool
--alphaConv = alphaConvUnder 
