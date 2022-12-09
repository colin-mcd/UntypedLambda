module Main where
import System.Environment (getArgs, getProgName)
import Data.List (intercalate)
import Parse (readTerms)
import Helpers
import SKI
import Struct

castIota :: Term -> Either String IotaTm
castIota (App t u) = pure IotaApp <*> castIota t <*> castIota u
castIota (Var "i") = pure Iota
castIota _ = Left "Invalid iota expression"

castSKI :: Term -> Either String SKITm
castSKI (App t u) = pure SKIApp <*> castSKI t <*> castSKI u
castSKI (Var "S") = pure (SKIVar S)
castSKI (Var "K") = pure (SKIVar K)
castSKI (Var "I") = pure (SKIVar I)
castSKI _ = Left "Invalid SKI expression"

data Encoding = LambdaCalculus | SKICalculus | IotaCalculus

readEncoding :: String -> Maybe Encoding
readEncoding "lambda" = Just LambdaCalculus
readEncoding "ski" = Just SKICalculus
readEncoding "iota" = Just IotaCalculus
readEncoding _ = Nothing

data CombOpts = CombOpts { fmEnc :: Encoding, toEnc :: Encoding }
defaultOpts = CombOpts { fmEnc = LambdaCalculus, toEnc = SKICalculus }

usage :: String -> String
usage progname = intercalate "\n" [
  "Usage:",
  (progname ++ " --from ENCODING --to ENCODING"),
  "Encodings:",
  "  lambda    Lambda calculus",
  "  ski       SKI combinator calculus",
  "  iota      Iota combinator calculus"
  ]

readOptions :: IO (Either String CombOpts)
readOptions =
  getArgs >>= \ as ->
  maybe (getProgName >>= return . Left . usage) (return . Right) (h as defaultOpts)
  where
    h :: [String] -> CombOpts -> Maybe CombOpts
    h [] o = Just o
    h ("--from" : fm : as) o =
      readEncoding fm >>= \ enc -> h as (o { fmEnc = enc })
    h ("--to"   : to : as) o =
      readEncoding to >>= \ enc -> h as (o { toEnc = enc })
    h _ o = Nothing

-- from-encoding -> to-encoding -> conversion IO
convertEnc :: Encoding -> Encoding -> IO ()
convertEnc LambdaCalculus outEnc =
  readTerms $ \ t -> case outEnc of
    LambdaCalculus -> show t
    SKICalculus -> show (convert t)
    IotaCalculus -> show (skiToIota (convert t))
convertEnc SKICalculus outEnc =
  readTerms $ \ t -> case outEnc of
    LambdaCalculus -> either id (show . skiToLam) (castSKI t)
    SKICalculus -> either id show (castSKI t)
    IotaCalculus -> either id (show . skiToIota) (castSKI t)
convertEnc IotaCalculus outEnc =
  readTerms $ \ t -> case outEnc of
    LambdaCalculus -> either id (show . iotaToLam) (castIota t)
    SKICalculus -> either id (show . convert . iotaToLam) (castIota t)
    IotaCalculus -> either id show (castIota t)

main :: IO ()
main =
  setBuffering >>
  guardIO readOptions >>= \ opts ->
  convertEnc (fmEnc opts) (toEnc opts)
