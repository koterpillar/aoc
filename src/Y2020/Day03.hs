module Y2020.Day03 where

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

gridMember :: Grid -> Position2 -> Bool
gridMember g (Position2 x y) = mapMember (Position2 (x `mod` width g) y) g

width :: Grid2 a -> Int
width g = xmax + 1
  where
    (_, Position2 xmax _) = boundsG g

steps :: Grid2 a -> Position2 -> [Position2]
steps g (Position2 dx dy) =
  takeWhile (\p -> pY p <= ymax) [Position2 (i * dx) (i * dy) | i <- [0 ..]]
  where
    (Position2 _ ymin, Position2 _ ymax) = boundsG g

countTrees :: Position2 -> Grid -> Int
countTrees direction g = length $ filter (gridMember g) $ steps g direction
  where
    (Position2 _ ymin, Position2 _ ymax) = boundsG g

countTrees1 = countTrees (Position2 3 1)

countTrees2 g =
  product $
  map
    (`countTrees` g)
    [Position2 1 1, Position2 3 1, Position2 5 1, Position2 7 1, Position2 1 2]

tasks =
  Tasks 2020 3 (CodeBlock 0) dotGridP [Task countTrees1 7, Task countTrees2 336]
