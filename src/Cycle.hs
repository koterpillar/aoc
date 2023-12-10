module Cycle
  ( Cycle(..)
  , cycleFind
  , cycleMap
  , cycleMerge
  , cycleMergeWith
  , cycleGenerate
  ) where

import qualified Data.Map as Map

import           Utils

data Cycle st =
  Cycle
    { cycleStart :: [st]
    , cycleLoop  :: [st]
    }
  deriving (Show, Eq, Ord)

cycleFind :: (Ord st, Show st) => (st -> st) -> st -> Cycle st
cycleFind next = go [] 0 Map.empty
  where
    go els n indices x =
      case Map.lookup x indices of
        Just i ->
          let (start, loop) = splitAt i $ reverse els
           in Cycle start loop
        Nothing -> go (x : els) (succ n) (Map.insert x n indices) (next x)

optimizeLoop :: (Eq st, Show st) => [st] -> [st]
optimizeLoop long = fromMaybe long $ findTuple (isPrefixOf long . cycle) splits
  where
    splits = map (`splitAt` long) [1 .. length long - 1]

cycleOptimize :: (Eq st, Show st) => Cycle st -> Cycle st
cycleOptimize (Cycle start loop) = Cycle start $ optimizeLoop loop

cycleMap :: (Ord b, Show b) => (a -> b) -> Cycle a -> Cycle b
cycleMap f (Cycle start loop) = cycleOptimize $ Cycle (map f start) (map f loop)

alignStart :: Show st => Int -> Cycle st -> Cycle st
alignStart len c@(Cycle start loop)
  | len >= length start = Cycle (start ++ extraStart) newLoop
  | otherwise =
    terror $ "cannot align start to " <> tshow len <> ": " <> tshow c
  where
    (extraStart, newLoop') = splitAt (len - length start) $ cycle loop
    newLoop = take (length loop) newLoop'

alignLoop :: Show st => Int -> Cycle st -> Cycle st
alignLoop len c@(Cycle start loop) =
  case divMod len (length loop) of
    (n, 0) -> Cycle start $ join $ replicate n loop
    _      -> terror $ "cannot align loop to " <> tshow len <> ": " <> tshow c

cycleZip :: Cycle a -> Cycle b -> Cycle (a, b)
cycleZip (Cycle s1 l1) (Cycle s2 l2) = Cycle (zip s1 s2) (zip l1 l2)

findStartLen :: Int -> Int -> Int -> Int -> Int
findStartLen s1 l1 s2 l2
  | s1 == s2 = s1
  | s1' < s2' = findStartLen s1' l1 s2 l2
  | otherwise = findStartLen s1 l1 s2' l2
  where
    s1' = s1 + l1
    s2' = s2 + l2

cycleMerge :: (Show a, Show b) => Cycle a -> Cycle b -> Cycle (a, b)
cycleMerge c1@(Cycle s1 l1) c2@(Cycle s2 l2) = cycleZip c1' c2'
  where
    newStartLen = findStartLen (length s1) (length l1) (length s2) (length l2)
    newLoopLen = lcm (length l1) (length l2)
    c1' = alignStart newStartLen $ alignLoop newLoopLen c1
    c2' = alignStart newStartLen $ alignLoop newLoopLen c2

cycleMergeWith ::
     (Show a, Show b, Show c, Ord c)
  => (a -> b -> c)
  -> Cycle a
  -> Cycle b
  -> Cycle c
cycleMergeWith f c1 c2 = cycleMap (uncurry f) $ cycleMerge c1 c2

cycleGenerate :: Cycle st -> [st]
cycleGenerate (Cycle start loop) = start ++ cycle loop
