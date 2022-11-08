module Main where
import Helpers
import SKI
import Struct

castSKI :: Term -> Either String SKITm
castSKI (App t u) = pure SKIApp <*> castSKI t <*> castSKI u
castSKI (Var "S") = pure (SKIVar S)
castSKI (Var "K") = pure (SKIVar K)
castSKI (Var "I") = pure (SKIVar I)
castSKI _ = Left "Invalid SKI expression"

main = readTerms (either id (show . skiToLam) . castSKI)
