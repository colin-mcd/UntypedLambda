module ANF where
import Struct
import Helpers
import Fresh
import Subst (freeVars, Binding)
import Data.Map (Map, singleton, delete, union, unions, insert)
import Data.List (intercalate)

-- Following https://matt.might.net/articles/a-normalization/

data AEXP =
    AEXPvar String
  | AEXPlam [String] EXP

data CEXP = CEXPapp AEXP [AEXP]

data EXP =
    EXPlet String CEXP EXP
  | EXPa AEXP
  | EXPc CEXP

instance Show AEXP where
  show (AEXPvar x) = x
  show (AEXPlam xs t) = parens (foldr (\ x t -> 'λ' : x ++ '.' : ' ' : t) (show t) xs)
instance Show CEXP where
  show (CEXPapp h as) = intercalate " " [show a | a <- h : as]
instance Show EXP where
  show (EXPlet x t u) = "let " ++ x ++ " = " ++ show t ++ " in " ++ show u
  show (EXPa a) = show a
  show (EXPc c) = show c

instance Binding AEXP where
  freeVars (AEXPvar x) = singleton x ()
  freeVars (AEXPlam xs t) = foldr delete (freeVars t) xs
instance Binding CEXP where
  freeVars (CEXPapp t us) = unions (freeVars t : [freeVars u | u <- us])
instance Binding EXP where
  freeVars (EXPlet x t u) =
    union (freeVars t) (delete x (freeVars u))
  freeVars (EXPa a) = freeVars a
  freeVars (EXPc c) = freeVars c

anf :: Term -> EXP
anf t = normalizeTerm (freeVars t) t

normalizeTerm :: Map String () -> Term -> EXP
normalizeTerm vs m = normalize vs m (either EXPa EXPc)

normalize :: Map String () -> Term -> (Either AEXP CEXP -> EXP) -> EXP
normalize vs m@(Lam _ _) k =
  let (params, body) = unlams m
      vs' = foldr (\x -> insert x ()) vs params in
    k (Left (AEXPlam params (normalizeTerm vs' body)))
normalize vs m@(App _ _) k =
  let (fn, ms) = unapps m in
    normalizeName vs fn (\t ->
      normalizeNames vs ms (\ts ->
        k (Right (CEXPapp t ts))))
normalize vs (Var x) k = k (Left (AEXPvar x))

normalizeName :: Map String () -> Term -> (AEXP -> EXP) -> EXP
normalizeName vs m k =
  normalize vs m (either k
    (\n' ->
       let t = fresh "t" vs in
         EXPlet t n' (k (AEXPvar t))))

normalizeNames :: Map String () -> [Term] -> ([AEXP] -> EXP) -> EXP
normalizeNames vs [] k = k []
normalizeNames vs (m : ms) k =
  normalizeName vs m
    (\t -> normalizeNames vs ms
      (\ts -> k (t : ts)))
