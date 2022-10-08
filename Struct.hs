module Struct where

------------------------------
-- Datatypes
------------------------------

data Term =
    Var String
  | App Term Term
  | Lam String Term

data TermDef = TermDef String Term

data Program = Program [TermDef] Term

------------------------------
-- Helpers
------------------------------

parens s = '(' : s ++ ")"

--doIf :: Bool -> (a -> a) -> a -> a
doIf True f = f
doIf False f = id

delimit s [] = ""
delimit s [x] = x
delimit s (x : xs) = x ++ s ++ delimit s xs


------------------------------
-- Printing
------------------------------

data ShowHist =
    ShowAppL
  | ShowAppR
  | ShowLam
  | ShowNone

-- Does printing term t need parens, given history s?
needParens ShowNone _ = False
needParens s (Var _) = False
needParens ShowLam  (App _ _) = False
needParens ShowAppL (App _ _) = False
needParens ShowAppR (App _ _) = True
needParens ShowLam  (Lam _ _) = False
needParens ShowAppL (Lam _ _) = True
needParens ShowAppR (Lam _ _) = False -- abc \x. x

showTermh (Var x) = x
showTermh (App t u) = showTerm ShowAppL t ++ " " ++ showTerm ShowAppR u
showTermh (Lam x t) = "\\" ++ x ++ ". " ++ showTerm ShowLam t

showTerm s t = doIf (needParens s t) parens (showTermh t)

instance Show Term where
  show = showTerm ShowNone

instance Show TermDef where
  show (TermDef x tm) = x ++ " = " ++ show tm ++ ";"

instance Show Program where
  show (Program ps tm) =
    delimit "\n\n" (map show ps) ++ "\n\n" ++ show tm
