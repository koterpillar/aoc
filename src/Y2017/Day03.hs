module Y2017.Day03
  ( tasks
  ) where

import           AOC
import           Grid
import           Utils

start = Position2 0 0

manhattanSpiral :: [(Position2, Grid2 Int)]
manhattanSpiral = manhattanSpiral' E (map1 start 1) start

manhattanSpiral' ::
     Direction4 -> Grid2 Int -> Position2 -> [(Position2, Grid2 Int)]
manhattanSpiral' curdir taken curpos =
  (curpos, taken) : manhattanSpiral' nextdir nexttaken nextpos
  where
    nextpos = walk curdir curpos
    wantdir = turnLeft curdir
    nexttaken = mapInsert nextpos (adjacentValues nextpos taken) taken
    nextwant = walk wantdir nextpos
    nextdir =
      if mapMember nextwant taken
        then curdir
        else wantdir

adjacentValues :: Position2 -> Grid2 Int -> Int
adjacentValues pt vals = sum $ mapMaybe (`mapLookup` vals) $ adjacent8 pt

part1 input =
  let (p, _) = manhattanSpiral !! (input - 1)
   in manhattanDistance p start

lastValue :: (Position2, Grid2 Int) -> Int
lastValue (pos, grid) = fromJust $ mapLookup pos grid

part2 input = head $ filter (> input) $ map lastValue manhattanSpiral

tasks =
  Tasks
    2017
    3
    (Inline "")
    integerP
    [ task part1 31 & taskScraper (Inline "1024")
    , task part2 806 & taskScraper (Inline "800")
    ]
