-- Big step reduction strategies
module Reduce where
import Struct
import Fresh
import Subst

-- A very helpful paper describing various reduction strategies:
-- https://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf

-- Call-by-name (CBN) reduction:
-- Don't reduce under lambdas
-- Substitute args into lambdas without reducing first
reduceCBN :: Term -> Term
reduceCBN (Var x) = Var x
reduceCBN (Lam x t) = Lam x t
reduceCBN (App t u) =
  case App (reduceCBN t) u of
    App (Lam x t) u -> reduceCBN ((x |-> u) t)
    tu -> tu

-- Call-by-value (CBV) reduction:
-- Don't reduce under lambdas
-- Reduce args first, then subtitute into lambdas
reduceCBV :: Term -> Term
reduceCBV (Var x) = Var x
reduceCBV (Lam x t) = Lam x t
reduceCBV (App t u) =
  case App (reduceCBV t) (reduceCBV u) of
    App (Lam x t) u -> reduceCBV ((x |-> u) t)
    tu -> tu

-- Normal order reduction:
-- Reduce under lambdas, if necessary
-- Substitute args into lambdas without reducing first
reduceNorm :: Term -> Term
reduceNorm (Var x) = Var x
reduceNorm (Lam x t) = Lam x (reduceNorm t)
reduceNorm (App t u) =
  case App (reduceCBN t) u of
    App (Lam x t) u -> reduceNorm ((x |-> u) t)
    App t u -> App (reduceNorm t) (reduceNorm u)
    _ -> error "shouldn't happen"

-- Applicative order reduction:
-- Reduce under lambdas, if necessary
-- Reduce args, then substitute into lambdas
reduceAppl :: Term -> Term
reduceAppl (Var x) = Var x
reduceAppl (Lam x t) = Lam x (reduceAppl t)
reduceAppl (App t u) =
  case App (reduceCBV t) (reduceAppl u) of
    App (Lam x t) u -> reduceAppl ((x |-> u) t)
    App t u -> App (reduceAppl t) u
    _ -> error "shouldn't happen"


{-
-- Reduce to normal form, normal order
normalForm :: Term -> Term
normalForm (Lam x t) = Lam x (normalForm t)
normalForm (Var x) = Var x
normalForm (App t u) = case App (betaReduce t) u of
  App (Lam x t) u -> normalForm ((x |-> u) t)
  App t u -> App t (normalForm u)
  _ -> error "shouldn't happen"
-}

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

--alphaConv :: Term -> Term -> Bool
--alphaConv = alphaConvUnder 

-- data EvalOrder = NormalOrder | ApplicativeOrder
-- data FullBeta  = YesUnderLambda | NoUnderLambda
-- data ReductionStrategy = ReductionStrategy {
--   evalOrder :: EvalOrder,
--   fullBeta :: FullBeta
-- }
-- cbnReduce :: 
-- reduce :: ReductionStrategy -> Term
