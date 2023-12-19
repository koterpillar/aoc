module Y2021.Day15 where

import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

type Grid = Grid2 Int

pathCost :: Grid -> [Position2] -> Int
pathCost grid path = sum (mapMaybe (`mapLookup` grid) path)

part1 grid =
  pathCost grid
    $ fromJust
    $ aStar moves distance heuristicDistance isGoal startPosition
  where
    (startPosition, endPosition) = boundsG grid
    isGoal = (== endPosition)
    distance p1 p2
      | p2 == startPosition = error "we can't go back to start position!"
      | otherwise = mapLookupE "destination" p2 grid
    heuristicDistance = manhattanDistance endPosition
    moves p = filter (`mapMember` grid) $ map (`walk` p) allDir4

enlarge :: Int -> Grid -> Grid
enlarge times grid = mapFromList $ concatMap copyP $ mapToList grid
  where
    (_, Position2 xmax ymax) = boundsG grid
    sx = xmax + 1
    sy = ymax + 1
    copyP (Position2 x y, v) =
      [ (Position2 (x + sx * dx) (y + sy * dy), wrap (v + dx + dy))
      | dx <- [0 .. times - 1]
      , dy <- [0 .. times - 1]
      ]
    wrap x
      | x > 9 = wrap (x - 9)
      | otherwise = x

part2 = part1 . enlarge 5

tasks = Tasks 2021 15 (CodeBlock 0) digitGridP [Task part1 40, Task part2 315]
