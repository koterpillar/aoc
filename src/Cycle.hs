module Cycle
  ( Cycle(..)
  , cycleFind
  , cycleMap
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

cycleMap :: (Ord b, Show b) => (a -> b) -> Cycle a -> Cycle b
cycleMap f (Cycle start loop) = Cycle (map f start) (optimizeLoop $ map f loop)

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

cycleGenerate :: Cycle st -> [st]
cycleGenerate (Cycle start loop) = start ++ cycle loop
