-- Removes all recursion in a program by using fix, with no substitution
module Inline where
import Struct
import Subst (freeVars, inFV)
import Helpers
import SCC
import Fresh
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
--import Data.Maybe (mapMaybe)

-- Y combinator
fix_y = either error id $ parseTerm "\\f. (\\x. f (x x)) (\\x. f (x x))"
-- Z combinator
fix_z = either error id $ parseTerm "\\f. (\\x. f (\\y. x x y)) (\\x. f (\\y. x x y))"

fvset :: Term -> Set.Set String
fvset = Set.fromList . Map.keys . freeVars

{-
  even n = if (iszero n) then true  else (odd  (pred n))
  odd  n = if (iszero n) then false else (even (pred n))
  t
becomes
  fix (\y. \f. f (y (\even. \odd. [even def])) (y (\even. \odd. [odd def]))) (\even. \odd. t)
or:
  fix (\y. \f. \x. y (\even. \odd. f [even def] [odd def]) x) (\even. \odd. t)
-}

inlineSCC :: Map.Map String Term -> Term -> [String] -> Term -> Term
inlineSCC defs fix [x] t
  | x `inFV` (defs Map.! x) =
    letm x (App fix (Lam x (defs Map.! x))) t
  | otherwise =
    letm x (defs Map.! x) t
inlineSCC defs fix xs t =
  let fvs = foldr (\x -> Map.insert x ()) (freeVars t) xs
      f = fresh "f" fvs -- pick new name
      v = fresh "v" fvs -- pick new name
      y = fresh "y" fvs -- pick new name
      body = Lam v (App (apps (Var f) [App (Var y) (lams xs (defs Map.! x)) | x <- xs]) (Var v)) -- safer?
--      body = Lam v (App (App (Var y) (lams xs (apps (Var f) [defs Map.! x | x <- xs]))) (Var v))
  in
    App (App fix (Lam y (Lam f body))) (lams xs t)

inline :: Program -> Term
inline (Program ds end) =
  let defs  = Map.fromList [(x, t) | TermDef x t <- ds]
      graph = fmap fvset defs -- get free vars in each def
      sccs  = scc graph -- get strongly-connected components
      fix = fresh "fix" defs
  in
    --letm fix fix_z (foldr (inlineSCC defs (Var fix)) end sccs)
    foldr (inlineSCC defs fix_z) end sccs

