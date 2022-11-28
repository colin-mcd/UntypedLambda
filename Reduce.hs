-- Big step reduction strategies
module Reduce where
import Data.Map (Map, lookup, delete)
import Struct
import Fresh
import Subst

-- \x. t x  ⇒  t  (where x \notin FV(t))
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

-- t  ⇒  \x. t x  (where x \notin FV(t))
etaExpand :: Term -> Term
etaExpand t =
  let x = fresh "x" (freeVars t) in
    Lam x (App t (Var x))


-- A very helpful paper describing various reduction strategies:
-- https://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf

-- Call-by-name (CBN) reduction:
--   Don't reduce under lambdas
--   Substitute args into lambdas without reducing first

-- Call-by-value (CBV) reduction:
--   Don't reduce under lambdas
--   Reduce args first, then subtitute into lambdas

-- Normal order reduction:
--   Reduce under lambdas, if necessary
--   Substitute args into lambdas without reducing first

-- Applicative order reduction:
--   Reduce under lambdas, if necessary
--   Reduce args, then substitute into lambdas

data ReductionStrategy =
    CallByName
  | CallByValue
  | NormOrder
  | ApplOrder

reduce :: Map String Term -> ReductionStrategy -> Term -> Term
reduce g s =
  case s of
    CallByName -> reduceCBN
    CallByValue -> reduceCBV
    NormOrder -> reduceNorm
    ApplOrder -> reduceAppl
  where
    -- if x maps to Var x, don't keep evaluating
    -- because that would cause a loop
    lookupG :: String -> Term
    lookupG x = case Data.Map.lookup x g of
      Nothing -> Var x
      Just t -> if t == Var x then t else reduce g s t
    
    reduceCBN (Var x)   = lookupG x
    reduceCBN (Lam x t) = Lam x t
    reduceCBN (App t u) =
      case App (reduceCBN t) u of
        App (Lam x t) u -> reduceCBN ((x |-> u) t)
        tu -> tu
    
    reduceCBV (Var x)   = lookupG x
    reduceCBV (Lam x t) = Lam x t
    reduceCBV (App t u) =
      case App (reduceCBV t) (reduceCBV u) of
        App (Lam x t) u -> reduceCBV ((x |-> u) t)
        tu -> tu
  
    reduceNorm (Var x)   = lookupG x
    reduceNorm (Lam x t) = Lam x (reduce (delete x g) NormOrder t)
    reduceNorm (App t u) =
      case App (reduceCBN t) u of
        App (Lam x t) u -> reduceNorm ((x |-> u) t)
        _ -> App (reduceNorm t) (reduceNorm u)
  
    reduceAppl (Var x)   = lookupG x
    reduceAppl (Lam x t) = Lam x (reduce (delete x g) ApplOrder t)
    reduceAppl (App t u) =
      case App (reduceCBV t) u of
        App (Lam x t) u -> reduceAppl ((x |-> reduceAppl u) t)
        _ -> App (reduceAppl t) (reduceAppl u)

step :: Map String Term -> ReductionStrategy -> Term -> Maybe Term
step g s =
  case s of
    CallByName -> stepCBN
    CallByValue -> stepCBV
    NormOrder -> stepNorm
    ApplOrder -> stepAppl
  where
    lookupG :: String -> Maybe Term
    lookupG x = case Data.Map.lookup x g of
      Nothing -> Nothing
      Just t  -> if t == Var x then Nothing else Just t
    
    stepCBN (Var x)   = lookupG x
    stepCBN (Lam x t) = Nothing
    stepCBN (App t u) =
      case t of
        Lam x t -> Just ((x |-> u) t)
        _       -> case stepCBN t of
          Just t  -> Just (App t u)
          Nothing -> case stepCBN u of
            Just u  -> Just (App t u)
            Nothing -> Nothing
  
    stepCBV (Var x)   = lookupG x
    stepCBV (Lam x t) = Nothing
    stepCBV (App t u) =
      case stepCBV u of
        Just u  -> Just (App t u)
        Nothing -> case t of
          Lam x t -> Just ((x |-> u) t)
          _       -> case stepCBV t of
            Just t  -> Just (App t u)
            Nothing -> Nothing
  
    stepNorm (Var x)   = lookupG x
    stepNorm (Lam x t) = Lam x <$> step (delete x g) s t
    stepNorm (App t u) =
      case t of
        Lam x t -> Just ((x |-> u) t)
        _       -> case stepCBN t of
          Just t  -> Just (App t u)
          Nothing -> case stepCBN u of
            Just u  -> Just (App t u)
            Nothing -> Nothing
  
    stepAppl (Var x)   = lookupG x
    stepAppl (Lam x t) = Lam x <$> step (delete x g) s t
    stepAppl (App t u) =
      case stepCBV u of
        Just u  -> Just (App t u)
        Nothing -> case t of
          Lam x t -> Just ((x |-> u) t)
          _       -> case stepCBV t of
            Just t  -> Just (App t u)
            Nothing -> Nothing

-- Generates a list of the term after each small step reduction
steps :: Map String Term -> ReductionStrategy -> Term -> [Term]
steps g s t = case step g s t of
  Nothing -> []
  Just t -> t : steps g s t
