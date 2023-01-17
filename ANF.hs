module ANF where
import Struct
import Helpers
--import Fresh
import Subst (freeVars, Binding)
import Data.Map (singleton, delete, union, unions)
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
anf t = normalizeTerm t
  where
    normalizeTerm :: Term -> EXP
    normalizeTerm m = normalize m (either EXPa EXPc)
    
    normalize :: Term -> (Either AEXP CEXP -> EXP) -> EXP
    normalize m@(Lam _ _) k =
      let (params, body) = unlams m in
        k (Left (AEXPlam params (normalizeTerm body)))
    normalize m@(App _ _) k =
      let (fn, ms) = unapps m in
        normalizeName 0 fn (\t ->
          normalizeNames ms (\ts ->
            k (Right (CEXPapp t ts))))
    normalize (Var x) k = k (Left (AEXPvar x))
    
    normalizeName :: Int -> Term -> (AEXP -> EXP) -> EXP
    normalizeName i m k =
      normalize m (either k
        (\n' ->
           let t = '_' : show i in
             EXPlet t n' (k (AEXPvar t))))
    
    normalizeNames :: [Term] -> ([AEXP] -> EXP) -> EXP
    normalizeNames [] k = k []
    normalizeNames (m : ms) k =
      normalizeName (length ms + 1) m
        (\t -> normalizeNames ms
          (\ts -> k (ts ++ [t])))
    
