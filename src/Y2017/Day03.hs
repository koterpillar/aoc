module Y2017.Day03 where

import qualified Data.Map as Map

import           AOC
import           Grid
import           Utils

start = Position2 0 0

manhattanSpiral :: [(Position2, Grid2 Int)]
manhattanSpiral = manhattanSpiral' E (Map.singleton start 1) start

manhattanSpiral' ::
     Direction4 -> Grid2 Int -> Position2 -> [(Position2, Grid2 Int)]
manhattanSpiral' curdir taken curpos =
  (curpos, taken) : manhattanSpiral' nextdir nexttaken nextpos
  where
    nextpos = walk curdir curpos
    wantdir = turnLeft curdir
    nexttaken = Map.insert nextpos (adjacentValues nextpos taken) taken
    nextwant = walk wantdir nextpos
    nextdir =
      if Map.member nextwant taken
        then curdir
        else wantdir

adjacentValues :: Position2 -> Grid2 Int -> Int
adjacentValues pt vals = sum $ mapMaybe (`Map.lookup` vals) $ adjacent8 pt

part1 input =
  let (p, _) = manhattanSpiral !! (input - 1)
   in manhattanDistance p start

lastValue :: (Position2, Grid2 Int) -> Int
lastValue (pos, grid) = fromJust $ Map.lookup pos grid

part2 input = head $ filter (> input) $ map lastValue manhattanSpiral

tasks =
  Tasks
    2017
    3
    (Inline "")
    integerP
    [TaskScraper (Inline "1024") part1 31, TaskScraper (Inline "800") part2 806]
