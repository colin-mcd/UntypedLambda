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
  } deriving Show

data DiffPath =
    HeadDiff -- head var is different
  | ArgsDiff -- number of args is different
  | ChildDiff Int DiffPath -- difference nested somewhere in nth arg
  deriving Show

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
      let (bs1', bs2') = h d bs1 bs2 in
        (b1 : bs1', b2 : bs2')
    h d b1 b2 = (b1, b2)
etaEquatePath t1 t2 p = etaEquate t1 t2

rotate :: Int -> BohmTree
rotate k =
  Node (succ k) (succ k) (nfoldl k [] (\ k' b -> Node (succ k) (succ k') [] : b))

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
constructPath :: BohmTree -> BohmTree -> Maybe (DiffPath, BohmTree, BohmTree)
constructPath (Node _ 0 _) _ = Nothing
constructPath _ (Node _ 0 _) = Nothing
constructPath t1 t2 = uncurry constructPath' (etaEquate t1 t2)
  where
    h :: Int -> [BohmTree] -> [BohmTree] -> Maybe (DiffPath, [BohmTree], [BohmTree])
    h n (b1 : bs1) (b2 : bs2) =
      maybe (fmap (\ (p, bs1', bs2') -> (p, b1 : bs1', b2 : bs2'))
                  (h (succ n) bs1 bs2))
            (\ (p, b1', b2') -> Just (ChildDiff n p, b1' : bs1, b2' : bs2))
            (constructPath b1 b2)
    h _ _ _ = Nothing
    
    constructPath' :: BohmTree -> BohmTree -> Maybe (DiffPath, BohmTree, BohmTree)
    constructPath' t1@(Node n1 i1 b1) t2@(Node n2 i2 b2)
      | i1 /= i2 = Just (HeadDiff, t1, t2)
      | length b1 /= length b2 = Just (ArgsDiff, t1, t2)
      | otherwise = fmap (\ (p, b1', b2') -> (p, Node n1 i1 b1', Node n2 i2 b2')) (h 0 b1 b2)

constructDelta :: BohmTree -> BohmTree -> DiffPath -> [BohmTree]
constructDelta (Node n1 i1 b1) (Node n2 i2 b2) HeadDiff =
  nfoldl n1 [] $ \ m -> (:) $
    if succ m == i1 then
      Node (2 + length b1) (1 + length b1) [] -- \... \x. \y. x
    else if succ m == i2 then
      Node (2 + length b2) (2 + length b2) [] -- \... \x. \y. y
    else
      Node 1 1 [] -- \x. x
constructDelta (Node n1 i1 b1) (Node n2 i2 b2) ArgsDiff =
  let l1 = length b1
      l2 = length b2
      lM = max l1 l2
      l = abs (l1 - l2)
  in
    nfoldl n1
      (nfoldr l [Node (2 + l) ((if l1 > l2 then 1 else 2) + l) []] -- \... \x. x (\... \a. \b. a) or \... \x. x (\... \a. \b. b)
        (\ l' rest -> rest ++
          [if succ l' == l then
             Node 2 (if l1 > l2 then 2 else 1) [] -- \x. \y. x or \x. \y. y
           else
             Node 1 1 [] -- \x. x
          ]))
      (\ n' -> (:)
        (if succ n' == i1 then
           Node (succ lM) (succ lM) [] -- \... \ x. x
         else
           Node 1 1 [] -- \x. x
        ))
constructDelta t1@(Node n1 i1 b1) t2@(Node n2 i2 b2) (ChildDiff d p) =
  let t1' = b1 !! d
      t2' = b2 !! d
      Node n1' i1' b1' = t1'
      Node n2' i2' b2' = t2'
  in
    if not (occursInPath i1 t1' p) && not (occursInPath i2 t2' p) then
      setNth (pred i1) (Node (length b1) (succ d) []) (constructDelta t1' t2' p)
    else
      let km = max (greatestApps i1 t1) (greatestApps i2 t2)
          t1'' = rotateBT i1 (toGreatestEta i1 km t1)
          t2'' = rotateBT i2 (toGreatestEta i2 km t2)
          (t1''', t2''') = etaEquatePath t1'' t2'' (ChildDiff d p)
          p' = ChildDiff d (adjustPath i1 t1' p)
      in
        setNth (pred i1) (rotate km) (constructDelta t1''' t2''' p')

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
  let t1' = constructBT (reduce ctxt NormOrder t1)
      t2' = constructBT (reduce ctxt NormOrder t2)
      p = constructPath t1' t2'
  in
    --error (show p)
    flip fmap p $ \ (p, t1, t2) ->
      reconstruct (Node 1 1 (map (etaExpandBT' 0) (constructDelta t1 t2 p)))
