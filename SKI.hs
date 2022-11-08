module SKI where
import Struct
import Subst
import Data.Map (member, singleton, union, empty, delete)

data SKIComb = S | K | I
  deriving (Eq, Show)

data SKITm = SKIVar SKIComb | SKIApp SKITm SKITm

data SKITmh =
    SKIVarh String
  | SKISKIh SKIComb
  | SKIApph SKITmh SKITmh
  | SKILamh String SKITmh

instance Binding SKITmh where
  freeVars (SKIVarh x) = singleton x ()
  freeVars (SKISKIh ski) = empty
  freeVars (SKIApph t u) = freeVars t `union` freeVars u
  freeVars (SKILamh x t) = delete x (freeVars t)

-- showSKI :: Bool -> SKITm -> String
-- Bool represents if we need parens for non-var
showSKI _ (SKIVar ski) = show ski
showSKI appr (SKIApp t u) = doIf appr parens (showSKI False t ++ " " ++ showSKI True u)

instance Show SKITm where
  show = showSKI False

toIntermediateSKI :: Term -> SKITmh
toIntermediateSKI (Var x) = SKIVarh x
toIntermediateSKI (App t u) = SKIApph (toIntermediateSKI t) (toIntermediateSKI u)
toIntermediateSKI (Lam x t) = SKILamh x (toIntermediateSKI t)

fromIntermediateSKI :: SKITmh -> Maybe SKITm
fromIntermediateSKI (SKIVarh x) = Nothing
fromIntermediateSKI (SKISKIh ski) = Just (SKIVar ski)
fromIntermediateSKI (SKIApph t u) =
  pure SKIApp <*> fromIntermediateSKI t <*> fromIntermediateSKI u
fromIntermedaiteSKI (SKILamh x t) = Nothing

--backToIntermediateSKI :: SKITm -> SKITmh
--backToIntermediateSKI (SKIVar ski) = SKISKIh ski
--backToIntermediateSKI (SKIApp t u) = SKIApph (backToIntermediateSKI t) (backToIntermediateSKI u)

convert' :: SKITmh -> SKITmh
convert' (SKIVarh x) = SKIVarh x
convert' (SKISKIh ski) = SKISKIh ski
convert' (SKIApph t u) = SKIApph (convert' t) (convert' u)
convert' (SKILamh x t)
  | x `inFV` t = case t of
      SKIVarh _ -> SKISKIh I
      SKISKIh _ -> error "This shouldn't happen"
      SKIApph t u -> SKIApph (SKIApph (SKISKIh S) (convert' (SKILamh x t))) (convert' (SKILamh x u))
      SKILamh _ _ ->
        convert' (SKILamh x (convert' t))
  | otherwise =
    SKIApph (SKISKIh K) (convert' t)

convert'' :: Term -> Maybe SKITm
convert'' = fromIntermediateSKI . convert' . toIntermediateSKI

convert :: Term -> SKITm
convert t = maybe (error ("Failed to convert " ++ show t ++ " to SKI")) id (convert'' t)

reduceSKI :: SKITm -> SKITm
reduceSKI (SKIVar ski) = SKIVar ski
reduceSKI (SKIApp t u) =
  case SKIApp (reduceSKI t) u of
    r@(SKIApp (SKIVar I) u) -> reduceSKI u
    r@(SKIApp (SKIApp (SKIVar K) t) u) -> reduceSKI t
    r@(SKIApp (SKIApp (SKIApp (SKIVar S) t) u) v) -> reduceSKI (SKIApp (SKIApp t v) (SKIApp u v))
    r -> r

data IotaTm = Iota | IotaApp IotaTm IotaTm

skiToIota :: SKITm -> IotaTm
skiToIota (SKIApp t u) = IotaApp (skiToIota t) (skiToIota u)
skiToIota (SKIVar S) = IotaApp Iota (IotaApp Iota (IotaApp Iota (IotaApp Iota Iota)))
skiToIota (SKIVar K) =               IotaApp Iota (IotaApp Iota (IotaApp Iota Iota))
skiToIota (SKIVar I) =                                           IotaApp Iota Iota

skiToLam :: SKITm -> Term
skiToLam (SKIApp t u) = App (skiToLam t) (skiToLam u)
skiToLam (SKIVar S) =
  Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))
skiToLam (SKIVar K) = Lam "x" $ Lam "y" $ Var "x"
skiToLam (SKIVar I) = Lam "x" $ Var "x"

iotaToLam :: IotaTm -> Term
iotaToLam (IotaApp t u) = App (iotaToLam t) (iotaToLam u)
iotaToLam Iota =
  Lam "f" $ App (App (Var "f")
                  (Lam "x" $ Lam "y" $ Lam "z" $
                    App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))
                (Lam "x" $ Lam "y" $ Var "x")
