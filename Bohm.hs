-- Implementation of Bohm's Theorem (http://www.di.unito.it/~dezani/papers/dgp.pdf)
module Bohm (makeDiscriminator) where
import Data.Map (Map, (!?), insert, empty)
import Struct
import Reduce
import Helpers

-- I'm not actually sure if this is the correct name for these trees...
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

-- Increases all node n's and i's greater than a threshold g
etaExpandBT' :: Int -> BohmTree -> BohmTree
etaExpandBT' g (Node n i b) =
  Node (succ n) (if i >= g then succ i else i) (map (etaExpandBT' g) b)

-- Eta-expands a Bohm tree
etaExpandBT :: BohmTree -> BohmTree
etaExpandBT t =
  let Node n i b = etaExpandBT' (succ (btN t)) t in
    Node n i (b ++ [Node n n []])

-- Shallow eta-expansion to match two tree's number of lambdas
etaEquate :: BohmTree -> BohmTree -> (BohmTree, BohmTree)
etaEquate t1 t2 =
  (nfold (btN t2 - btN t1) t1 etaExpandBT,
   nfold (btN t1 - btN t2) t2 etaExpandBT)

-- etaEquates all nodes along a path
etaEquatePath :: BohmTree -> BohmTree -> DiffPath -> (BohmTree, BohmTree)
etaEquatePath (Node n1 i1 b1) (Node n2 i2 b2) (ChildDiff d p) =
  let (b1d, b2d) = etaEquatePath (b1 !! d) (b2 !! d) p in
    etaEquate (Node n1 i1 (setNth d b1d b1)) (Node n2 i2 (setNth d b2d b2))
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

-- Rotate all node j's (assumed to have been eta-expanded to the greatest number of apps)
rotateBT :: Int -> BohmTree -> BohmTree
rotateBT j (Node n i b)
  | i == j = Node (succ n) (succ n) (map (etaExpandBT' (succ n)) (map (rotateBT j) b))
  | otherwise = Node n i (map (rotateBT j) b)

-- Finds the greatest number of args a head k ever has along a path
greatestApps :: Int -> BohmTree -> DiffPath -> Int
greatestApps k bt = h 0 bt where
  h :: Int -> BohmTree -> DiffPath -> Int
  h m (Node n i b) (ChildDiff d p) =
    h (if i == k then max m (length b) else m) (b !! d) p
  h m (Node n i b) headOrArgsDiff =
    if i == k then max m (length b) else m
-- greatestApps k bt = greatestApps' k [bt]
--   where
--     greatestApps' :: Int -> [BohmTree] -> Int
--     greatestApps' k [] = 0
--     greatestApps' k (Node n i b : bs) =
--       let gab = max (greatestApps' k b) (greatestApps' k bs) in
--         if k == i then max (length b) gab else gab

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
constructPathBFS :: BohmTree -> BohmTree -> Maybe (DiffPath, BohmTree, BohmTree)
constructPathBFS (Node _ 0 _) _ = Nothing
constructPathBFS _ (Node _ 0 _) = Nothing
constructPathBFS t1 t2 = etaPath <$> constructPath' [([], t1, t2)]
  where
    etaPath p =
      let (t1', t2') = etaEquatePath t1 t2 p in
        (p, t1', t2')
    
    appendRevPath :: [Int] -> DiffPath -> DiffPath
    appendRevPath cs p = foldl (flip ChildDiff) p cs
    
    constructPath' :: [([Int], BohmTree, BohmTree)] -> Maybe DiffPath
    constructPath' [] = Nothing
    constructPath' ((cs, t1@(Node n1 i1 b1), t2@(Node n2 i2 b2)) : queue)
      | n1 /= n2 = let (t1', t2') = etaEquate t1 t2 in constructPath' ((cs, t1', t2') : queue)
      | i1 /= i2 = Just (appendRevPath cs HeadDiff)
      | length b1 /= length b2 = Just (appendRevPath cs ArgsDiff)
      | otherwise =
        constructPath' (queue ++ zip3 [h : cs | h <- [0..]] b1 b2)

constructDelta :: BohmTree -> BohmTree -> DiffPath -> [BohmTree]
constructDelta (Node n1 i1 b1) (Node n2 i2 b2) HeadDiff =
  let l1 = length b1
      l2 = length b2
  in
    setNth (pred i1) (selectCombinator (2 + l1) (1 + l1)) $ -- \... t f. t
    setNth (pred i2) (selectCombinator (2 + l2) (2 + l2)) $ -- \... t f. f
    replicate n1 trivialCombinator -- a bunch of (\x. x) args (n1 of them, to be exact)
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
  let t1d = b1 !! d
      t2d = b2 !! d
  in
    if not (occursInPath i t1d p) && not (occursInPath i t2d p) then
      setNth (pred i) (selectCombinator (length b1) (succ d)) (constructDelta t1d t2d p)
    else
      let km = max (greatestApps i t1 (ChildDiff d p)) (greatestApps i t2 (ChildDiff d p))
          t1' = toGreatestEta i km t1
          t2' = toGreatestEta i km t2
          t1'' = rotateBT i t1'
          t2'' = rotateBT i t2'
          p' = ChildDiff d (adjustPath i t1d p)
          (t1''', t2''') = etaEquatePath t1'' t2'' p'
      in
        setNth (pred i) (rotateCombinator km) (constructDelta t1''' t2''' p')

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
    constructPathBFS t1'' t2'' >>= \ (p, t1''', t2''') ->
    Just (reconstruct (Node 1 1 (map (etaExpandBT' 0) (constructDelta t1''' t2''' p))))
