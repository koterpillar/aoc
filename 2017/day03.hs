module Day03 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Utils

manhattanSpiral :: Int -> (Position2, Map Position2 Int)
manhattanSpiral steps = manhattanSpiral' steps E (Map.singleton (Position2 0 0) 1) (Position2 0 0)

manhattanSpiral' :: Int -> Direction4 -> Map Position2 Int -> Position2 -> (Position2, Map Position2 Int)
manhattanSpiral' 0 _ taken pos = (pos, taken)
manhattanSpiral' steps curdir taken curpos =
  manhattanSpiral' (steps - 1) nextdir nexttaken nextpos
  where
    nextpos = walk curdir curpos
    wantdir = turnLeft curdir
    nexttaken = Map.insert nextpos (adjancedValues nextpos taken) taken
    nextwant = walk wantdir nextpos
    nextdir =
      if Map.member nextwant taken
        then curdir
        else wantdir

adjancedValues :: Position2 -> Map Position2 Int -> Int
adjancedValues pt vals = sum $ mapMaybe (`Map.lookup` vals) $ adjancedPoints pt

adjancedPoints (Position2 x y) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]
  pure $ Position2 x' y'

maxVal :: Map Position2 Int -> Int
maxVal = maximum . Map.elems
