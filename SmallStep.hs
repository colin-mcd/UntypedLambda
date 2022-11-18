module SmallStep where
import Struct
import Subst
import Data.Map (Map, (!?))

-- Apply a single small-step reduction, if possible
stepReduce :: Term -> Maybe Term
stepReduce (Var x) = Nothing
stepReduce (Lam x t) = Nothing
stepReduce (App (Lam x t) u) = Just ((x |-> u) t)
stepReduce (App t u) =
  case (stepReduce t, stepReduce u) of
    (Just t, _) -> Just (App t u)
    (_, Just u) -> Just (App t u)
    (Nothing, Nothing) -> Nothing

-- List what the input term looks like after a single
-- reduction step, repeating until in normal form
reductions :: Term -> [Term]
reductions t = case stepReduce t of
  Nothing -> []
  Just t -> t : reductions t

-------------------------------------------------------------

-- Apply a single small-step reduction under a global context
-- Could be simplified into just one function,
-- but for readability I've left them separate
stepReduceUnder :: Map String Term -> Term -> Maybe Term
stepReduceUnder g (Var x) = g !? x
stepReduceUnder g (Lam x t) = Nothing
stepReduceUnder g (App (Lam x t) u) = Just ((x |-> u) t)
stepReduceUnder g (App t u) =
  case (stepReduceUnder g t, stepReduceUnder g u) of
    (Just t, _) -> Just (App t u)
    (_, Just u) -> Just (App t u)
    (Nothing, Nothing) -> Nothing

-- Under a global context
reductionsUnder :: Map String Term -> Term -> [Term]
reductionsUnder g t = case stepReduceUnder g t of
  Nothing -> []
  Just t -> t : reductionsUnder g t
