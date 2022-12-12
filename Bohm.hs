-- Implementation of Bohm's Theorem (http://www.di.unito.it/~dezani/papers/dgp.pdf)
module Bohm (makeDiscriminator) where
import Data.Map (Map, (!?), insert, empty)
import Struct
import Reduce
import Helpers

data BohmTree = Node {
  btN :: Int, -- number of lambdas currently bound
  btI :: Int, -- head variable
  btB :: [BohmTree] -- list of app args
  } deriving (Show, Eq)

data DiffPath =
    HeadDiff -- head var is different
  | ArgsDiff -- number of args is different
  | ChildDiff Int DiffPath -- difference nested somewhere in nth arg
  deriving (Show, Eq)

etaExpandBT' :: Int -> BohmTree -> BohmTree
etaExpandBT' g (Node n i b) =
  Node (succ n) (if i >= g then succ i else i) (map (etaExpandBT' g) b)

etaExpandBT :: BohmTree -> BohmTree
etaExpandBT t =
  let Node n i b = etaExpandBT' (succ (btN t)) t in
    Node n i (b ++ [Node n n []])

etaEquate :: BohmTree -> BohmTree -> (BohmTree, BohmTree)
etaEquate t1 t2 =
  (nfold (btN t2 - btN t1) t1 etaExpandBT,
   nfold (btN t1 - btN t2) t2 etaExpandBT)

etaEquatePath :: BohmTree -> BohmTree -> DiffPath -> (BohmTree, BohmTree)
etaEquatePath (Node n1 i1 b1) (Node n2 i2 b2) (ChildDiff d p) =
  let (b1', b2') = h d b1 b2 in
    etaEquate (Node n1 i1 b1') (Node n2 i2 b2')
  where
    h :: Int -> [BohmTree] -> [BohmTree] -> ([BohmTree], [BohmTree])
    h 0 (b1 : bs1) (b2 : bs2) =
      let (b1', b2') = etaEquatePath b1 b2 p in
        (b1' : bs1, b2' : bs2)
    h sd (b1 : bs1) (b2 : bs2) =
      let (bs1', bs2') = h (pred sd) bs1 bs2 in
        (b1 : bs1', b2 : bs2')
    h d b1 b2 = (b1, b2)
etaEquatePath t1 t2 p = etaEquate t1 t2

-- Creates the "rotate" combinator, for input k:
-- \x1 x2 ... x_k x{k+1}. x{k+1} x1 x2 ... xk
-- (I think this is effectively a kind of CPS-like transformation)
rotateCombinator :: Int -> BohmTree
rotateCombinator k =
  Node (succ k) (succ k) [Node (succ k) k' [] | k' <- [1..k]]

-- On input m n, Selects the nth of m args:
-- \x1 x2 ... xm. xn
selectCombinator :: Int -> Int -> BohmTree
selectCombinator m n = Node m n []

-- Creates \x. x for args that never get used
trivialCombinator :: BohmTree
trivialCombinator = Node 1 1 []

rotateBT :: Int -> BohmTree -> BohmTree
rotateBT k (Node n i b)
  | i == k = Node (succ n) (succ n) (map (etaExpandBT' (succ n)) (map (rotateBT k) b))
  | otherwise = Node n i (map (rotateBT k) b)

-- Finds the greatest number of args a head k ever has
greatestApps :: Int -> BohmTree -> Int
greatestApps k bt = greatestApps' k [bt]
  where
    greatestApps' :: Int -> [BohmTree] -> Int
    greatestApps' k [] = 0
    greatestApps' k (Node n i b : bs) =
      let gab = max (greatestApps' k b) (greatestApps' k bs) in
        if k == i then max (length b) gab else gab

-- Eta-expand all head k nodes to have m args
toGreatestEta :: Int -> Int -> BohmTree -> BohmTree
toGreatestEta k m (Node n i b)
  | k == i = nfold (m - length b) (Node n i (map (toGreatestEta k m) b)) etaExpandBT
  | otherwise = Node n i (map (toGreatestEta k m) b)

-- Determines if there is a node with head k somewhere following path in a tree
occursInPath :: Int -> BohmTree -> DiffPath -> Bool
occursInPath k (Node n i b) (ChildDiff d p) =
  k == i || occursInPath k (b !! d) p
occursInPath k (Node n i b) p = k == i

-- Changes all ArgsDiff with head k to HeadDiff
adjustPath :: Int -> BohmTree -> DiffPath -> DiffPath
adjustPath k (Node n i b) (ChildDiff d p) = ChildDiff d (adjustPath k (b !! d) p)
adjustPath k (Node n i b) ArgsDiff        = if k == i then HeadDiff else ArgsDiff
adjustPath k (Node n i b) HeadDiff        = HeadDiff

-- Constructs a BohmTree from a term,
-- where a 0 head represents a var bound
-- outside the scope of the original term
constructBT :: Term -> BohmTree
constructBT = h 0 empty Node where
  h :: Int -> Map String Int -> (Int -> Int -> [BohmTree] -> BohmTree) -> Term -> BohmTree
  h n vm f (Var x) = f n (maybe 0 id (vm !? x)) []
  h n vm f (App t u) =
    let u' = h n vm Node u in
      h n vm (\ n i b -> f n i (b ++ [u'])) t
  h n vm f (Lam x t) = h (succ n) (insert x (succ n) vm) f t

-- Finds a difference path in two BohmTrees, if there is one
-- Returns the path, along with the two eta-expanded Bohm trees
constructPath :: BohmTree -> BohmTree -> Maybe (DiffPath, BohmTree, BohmTree)
constructPath (Node _ 0 _) _ = Nothing
constructPath _ (Node _ 0 _) = Nothing
constructPath t1 t2 = uncurry constructPath' (etaEquate t1 t2)
  where
    h :: Int -> [BohmTree] -> [BohmTree] -> Maybe (DiffPath, [BohmTree], [BohmTree])
    h n (b1 : bs1) (b2 : bs2) =
      case constructPath b1 b2 of
        Nothing -> h (succ n) bs1 bs2 >>= \ (p, bs1', bs2') -> Just (p, b1 : bs1', b2 : bs2')
        Just (p, b1', b2') -> Just (ChildDiff n p, b1' : bs1, b2' : bs2)
    h n [] [] = Nothing
    h _ _ _ = error "constructPath h should get lists of equal length"
    
    constructPath' :: BohmTree -> BohmTree -> Maybe (DiffPath, BohmTree, BohmTree)
    constructPath' t1@(Node n1 i1 b1) t2@(Node n2 i2 b2)
      | i1 /= i2 = Just (HeadDiff, t1, t2)
      | length b1 /= length b2 = Just (ArgsDiff, t1, t2)
      | otherwise =
          h 0 b1 b2 >>= \ (p, b1', b2') ->
            Just (p, Node n1 i1 b1', Node n2 i2 b2')

constructDelta :: BohmTree -> BohmTree -> DiffPath -> [BohmTree]
constructDelta (Node n1 i1 b1) (Node n2 i2 b2) HeadDiff =
  nfoldl n1 [] $ \ m -> (:) $
    if succ m == i1 then
      selectCombinator (2 + length b1) (1 + length b1) -- \... \x. \y. x
    else if succ m == i2 then
      selectCombinator (2 + length b2) (2 + length b2) -- \... \x. \y. y
    else
      trivialCombinator
constructDelta (Node n i b1) (Node _ _ b2) ArgsDiff =
  let l1 = length b1
      l2 = length b2
      lM = max l1 l2
      l = abs (l1 - l2)
  in
    nfoldl n
      (nfoldr l [selectCombinator (2 + l) (if l1 > l2 then l + 1 else l + 2)] -- \... \x. x (\... \a. \b. a) or \... \x. x (\... \a. \b. b)
        (\ l' rest -> rest ++
          [if succ l' == l then
             selectCombinator 2 (if l1 > l2 then 2 else 1) -- \x. \y. x or \x. \y. y
           else
             trivialCombinator
          ]))
      (\ n' -> (:)
        (if succ n' == i then
           selectCombinator (succ lM) (succ lM) -- \... \x. x
         else
           trivialCombinator
        ))
constructDelta t1@(Node n i b1) t2@(Node _ _ b2) (ChildDiff d p) =
  --if n1 /= n2 then error ("n1 should equal n2, but " ++ show n1 ++ " /= " ++ show n2) else
  --if i1 /= i1 then error ("i1 should equal i2, but " ++ show i1 ++ " /= " ++ show i2) else
  let t1d = b1 !! d
      t2d = b2 !! d
  in
    if not (occursInPath i t1d p) && not (occursInPath i t2d p) then
      setNth (pred i) (selectCombinator (length b1) (succ d)) (constructDelta t1d t2d p)
    else
      let km = max (greatestApps i t1) (greatestApps i t2)
          t1' = toGreatestEta i km t1 -- eta expand to match t2, if necessary
          t2' = toGreatestEta i km t2 -- eta expand to match t1, if necessary
          t1'' = rotateBT i t1'
          t2'' = rotateBT i t2'
          p' = ChildDiff d (adjustPath i t1d p)
          --p' = ChildDiff d (adjustPath i (btB t1'' !! d) p)
          (t1''', t2''') = etaEquatePath t1'' t2'' (ChildDiff d p')
      in
        --if p /= p' then error ("Path adjusted i=" ++ show i ++ "\nt1=" ++ show t1 ++ "\nt2=" ++ show t2 ++ "\nt1'''=" ++ show t1''' ++ "\nt2'''=" ++ show t2''' ++ "\nold: " ++ show p ++ "\nnew:" ++ show p') else
        setNth (pred i) (rotateCombinator km) (constructDelta t1''' t2''' p')
        --setNth i (rotateCombinator km) (constructDelta t1''' t2''' p')

-- Converts a BohmTree back into a Term
reconstruct :: BohmTree -> Term
reconstruct = h 0 where
  mkvar :: Int -> String
  mkvar n = "x" ++ show n

  h :: Int -> BohmTree -> Term
  h m (Node n i b) = nfoldl (n - m) (a n (Var (mkvar i)) b) (\ nm -> Lam (mkvar (succ (m + nm))))
  
  a :: Int -> Term -> [BohmTree] -> Term
  a n t [] = t
  a n t (b : bs) = a n (App t (h n b)) bs

-- Given two terms t and u, returns a discriminator term f if one exists, such that
--   f t   ⇒   \x. \y. x
--   f u   ⇒   \x. \y. y
makeDiscriminator :: Map String Term -> Term -> Term -> Maybe Term
makeDiscriminator ctxt t1 t2 =
  let t1' = reduce ctxt NormOrder t1
      t2' = reduce ctxt NormOrder t2
      t1'' = constructBT t1'
      t2'' = constructBT t2'
  in
    constructPath t1'' t2'' >>= \ (p, t1''', t2''') ->
    --error (show p ++ "\n" ++ show t1''' ++ "\n" ++ show t2''' ++ "\n" ++ show [t1'' == t1''', t2'' == t2'''])
    Just (reconstruct (Node 1 1 (map (etaExpandBT' 0) (constructDelta t1''' t2''' p))))


{-
Error: doesn't work properly on
λn. λz. n (λx. x) (λz0. z0)
λn. λz. n (λx. x) (λz0. n (λs1. λz1. z1))
-}
