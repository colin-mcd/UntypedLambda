module Main where
import Struct
import Helpers
import SKI

castIota :: Term -> Either String IotaTm
castIota (App t u) = pure IotaApp <*> castIota t <*> castIota u
castIota (Var "i") = pure Iota
castIota _ = Left "Invalid iota expression"

main = readTerms (either id (show . iotaToLam) . castIota)
