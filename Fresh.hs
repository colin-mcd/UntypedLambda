module Fresh where
import Data.Map (Map, insert, member)
import Data.Char

data SplitVar = SplitVar String Int String
succSplitVar (SplitVar pre i suf) = SplitVar pre (succ i) suf

rejoin :: SplitVar -> String
rejoin (SplitVar pre i suf) = pre ++ show i ++ suf

-- Splits abc14'' into SplitVar "abc" 14 "\'\'"
splitVar :: String -> SplitVar
splitVar x =
  let (pre, i, suf) = h True (reverse x)
      pre' = reverse pre
      i' = reverse i
      suf' = reverse suf
      i'' = if null i' then 0 else succ (read i' :: Int)
  in
    SplitVar pre' i'' suf'
  where
    h :: Bool -> String -> (String, String, String)
    h b "" = ("", "", "")
    h True ('\'' : cs) =
      let (pre, i, suf) = h True cs in
        (pre, i, '\'' : suf)
    h True (c : cs) = h False (c : cs)
    h False (c : cs)
      | isDigit c =
        let (pre, i, suf) = h False cs in
          (pre, c : i, suf)
      | otherwise = (c : cs, "", "")

-- Given a map and a var, try new var names until it is no longer in the map
fresh :: String -> Map String a -> String
fresh x g = if member x g then h (splitVar x) else x
  where
    h x = let x' = rejoin x in if member x' g then h (succSplitVar x) else x'

freshs :: [String] -> Map String a -> [String]
freshs xs g =
  fst (foldr (\ x (xs', g') -> let x' = fresh x g' in (x':xs', insert x' () g')) ([], () <$ g) xs)
