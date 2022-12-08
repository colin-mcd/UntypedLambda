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
    lookupG :: String -> ReductionStrategy -> Term
    lookupG x s = case Data.Map.lookup x g of
      Nothing -> Var x
      Just t -> if t == Var x then t else reduce g s t
    
    reduceCBN (Var x)   = lookupG x CallByName
    reduceCBN (Lam x t) = Lam x t
    reduceCBN (App t u) =
      case App (reduceCBN t) u of
        App (Lam x t) u -> reduceCBN ((x |-> u) t)
        tu -> tu
    
    reduceCBV (Var x)   = lookupG x CallByValue
    reduceCBV (Lam x t) = Lam x t
    reduceCBV (App t u) =
      case App (reduceCBV t) (reduceCBV u) of
        App (Lam x t) u -> reduceCBV ((x |-> u) t)
        tu -> tu
  
    reduceNorm (Var x)   = lookupG x NormOrder
    reduceNorm (Lam x t) = Lam x (reduce (delete x g) NormOrder t)
    reduceNorm (App t u) =
      case App (reduceCBN t) u of
        App (Lam x t) u -> reduceNorm ((x |-> u) t)
        _ -> App (reduceNorm t) (reduceNorm u)
  
    reduceAppl (Var x)   = lookupG x ApplOrder
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
        _       -> case stepCBN t of -- dig for a lambda
          Just t  -> Just (App t u)
          Nothing -> case stepNorm u of
            Just u  -> Just (App t u)
            Nothing -> Nothing
  
    stepAppl (Var x)   = lookupG x
    stepAppl (Lam x t) = Lam x <$> step (delete x g) s t
    stepAppl (App t u) =
      case stepAppl u of
        Just u  -> Just (App t u)
        Nothing -> case t of
          Lam x t -> Just ((x |-> u) t)
          _       -> case stepCBV t of -- dig for a lambda
            Just t  -> Just (App t u)
            Nothing -> Nothing

-- Generates a list of the term after each small step reduction
steps :: Map String Term -> ReductionStrategy -> Term -> [Term]
steps g s t = case step g s t of
  Nothing -> []
  Just t -> t : steps g s t

-- Highlights the diff between two terms
-- Intended for use with output of steps
diff :: Term -> Term -> String
diff old new = show (maybe new id (h old new)) where
  redstart = "\x1b[4m" -- "\x1b[31m"
  colorend = "\x1b[0m"
  highlight s = redstart ++ s ++ colorend
  h (Lam x1 t1) (Lam x2 t2)
    | x1 == x2 = fmap (Lam x2) (h t1 t2)
    -- TODO: alpha-rename t1?
    | otherwise = Just (Var (highlight (parens (show (Lam x2 t2)))))
  h (App t1 u1) (App t2 u2) =
    case h t1 t2 of
      Just t3 -> Just (App t3 u2)
      Nothing -> fmap (App t2) (h u1 u2)
  h (Var x1) (Var x2)
    | x1 == x2 = Nothing
    | otherwise = Just (Var (highlight x2))
  h t1 t2 = Just (Var (highlight (parens (show t2))))

stepsDiff :: Term -> [Term] -> [String]
stepsDiff old [] = [show old]
-- diff new old (not vice-versa) so that we highlight the redex to be reduced
stepsDiff old (new : next) = diff new old : stepsDiff new next
