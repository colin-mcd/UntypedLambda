module Subst where
import Struct
import Data.Map
import Fresh


class Binding a where
  freeVars :: a -> Map String ()

class Substitutable a where
  subst :: Map String a -> a -> a

  alpha :: a -> a
  alpha = subst empty

instance Binding Term where
  freeVars (Var x) = singleton x ()
  freeVars (App t u) = freeVars t `union` freeVars u
  freeVars (Lam x t) = delete x (freeVars t)

instance Substitutable Term where
  subst m (Var x) =
    case m !? x of
      Nothing -> Var x
      Just t -> t
  subst m (App t u) =
    App (subst m t) (subst m u)
  subst m (Lam x t) =
    let x' = fresh x m in
      Lam x' (subst (insert x (Var x') (insert x' (Var x') m)) t)

alphaUnder :: Map String a -> Term -> Term
alphaUnder = subst . mapWithKey (\ k _ -> Var k)

infix 9 |->
  
--(|->) :: Substitutable a => String -> a -> a -> a
--x |-> a = subst (singleton x a)

(|->) :: String -> Term -> Term -> Term
x |-> a = subst (insert x a (mapWithKey (\ k _ -> Var k) (freeVars a)))

inFV :: Binding a => String -> a -> Bool
x `inFV` a = x `member` freeVars a
