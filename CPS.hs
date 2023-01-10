module CPS where
import Struct
import Subst (freeVars, Binding)
import Helpers
import Fresh
import Data.Map (singleton, delete, union, unions, insert)

-- Following https://matt.might.net/articles/cps-conversion/

-- Atomic term
data ATerm =
    ALam [String] CTerm
  | AVar String

-- Complex term
data CTerm = CApp ATerm [ATerm]

instance Binding ATerm where
  freeVars (AVar x) = singleton x ()
  freeVars (ALam xs ct) = foldr delete (freeVars ct) xs
instance Binding CTerm where
  freeVars (CApp t us) = unions (freeVars t : [freeVars u | u <- us])

castCTerm :: CTerm -> Term
castCTerm (CApp t us) = apps (castATerm t) ([castATerm u | u <- us])

castATerm :: ATerm -> Term
castATerm (AVar x) = Var x
castATerm (ALam xs t) = lams xs (castCTerm t)

-- Direct-style translation
toCTerm :: Term -> CTerm
toCTerm t = let (h, as) = unapps t in CApp (toATerm h) [toATerm a | a <- as]
toATerm :: Term -> ATerm
toATerm t@(Lam _ _) = let (xs, body) = unlams t in ALam xs (toCTerm body)
toATerm (Var x) = AVar x
toATerm (App _ _) = error "toATerm doesn't handle App"

-- Naive CPS
cpsNaive :: Term -> Term
cpsNaive t =
  let k = fresh "k" (freeVars t) in
    Lam k (castCTerm (cpsT t (AVar k)))
  where
    cpsM :: Term -> ATerm
    cpsM (Lam x t) =
      let k = fresh "k" (freeVars t) in
        ALam [x, "k"] $ cpsT t (AVar "k")
    cpsM (Var x) = AVar x
    cpsM (App t u) = error "cpsNaive helper cpsM doesn't handle App"
    
    cpsT :: Term -> ATerm -> CTerm
    cpsT (App t u) k =
      let fvs = freeVars t `union` freeVars u `union` freeVars k
          xt = fresh "xt" fvs
          xu = fresh "xu" fvs
      in
        cpsT t (ALam [xt] (cpsT u (ALam [xu] (CApp (AVar xt) [AVar xu, k]))))
    cpsT t k = CApp k [cpsM t]

-- Higher-order
cpsHO :: Term -> Term
cpsHO t =
  castCTerm (cpsT t (\t' -> CApp t' []))
  where
    cpsM :: Term -> ATerm
    cpsM (Lam x t) =
      let k = fresh "k" (freeVars t) in
        ALam [x, k] (cpsT t (\t' -> CApp (AVar k) [t']))
    cpsM (Var x) = AVar x
    cpsM (App t u) = error "cpsHO helper cpsM doesn't handle App"
    
    cpsT :: Term -> (ATerm -> CTerm) -> CTerm
    cpsT (App t u) k =
      let v = fresh "v" (freeVars t `union` freeVars u) in
        cpsT t (\t' -> cpsT u (\u' -> CApp t' [u', ALam [v] (k (AVar v))]))
    cpsT t k = k (cpsM t)

-- Hybrid
cps :: Term -> ATerm -> Term
cps t cc = castCTerm (cpsTc t cc)
  where
    fresh' x fvs = fresh x (freeVars cc `union` fvs)
    
    cpsTk :: Term -> (ATerm -> CTerm) -> CTerm
    cpsTk (App t u) k =
      let v = fresh' "v" (freeVars (App t u)) in
        cpsTk t (\t' -> cpsTk u (\u' -> CApp t' [u', ALam [v] (k (AVar v))]))
    cpsTk t k = k (cpsM t)
    
    cpsTc :: Term -> ATerm -> CTerm
    cpsTc (App t u) c =
      cpsTk t (\t' -> cpsTk u (\u' -> CApp t' [u', c]))
    cpsTc t c = CApp c [cpsM t]

    cpsM :: Term -> ATerm
    cpsM (Lam x t) =
      let k = fresh' "k" (freeVars t) in
        ALam [x, k] (cpsTc t (AVar k))
    cpsM (Var x) = AVar x
    cpsM (App _ _) = error "cps helper cpsM doesn't handle App"
