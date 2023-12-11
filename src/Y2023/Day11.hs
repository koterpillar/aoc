module Y2023.Day11 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

parser :: Parser Text Grid
parser = dotGridP

distances :: Int -> Grid -> Int
distances expansion g =
  sum [distance g1 g2 | (g1:gs) <- tails galaxiesL, g2 <- gs]
  where
    galaxies = Map.keysSet g
    galaxiesL = Set.toList galaxies
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    ex = filter (none pX) [xmin .. xmax]
    ey = filter (none pY) [ymin .. ymax]
    none acc i = Set.notMember i $ Set.map acc galaxies
    distance p1 p2 = manhattan ex (pX p1) (pX p2) + manhattan ey (pY p1) (pY p2)
    manhattan gaps a b =
      sum $
      map
        (\i ->
           if i `elem` gaps
             then expansion
             else 1)
        [min a b + 1 .. max a b]

part2 :: Grid -> Int
part2 = distances 1000000

part1 :: Grid -> Int
part1 = distances 2

tasks =
  Tasks
    2023
    11
    (CodeBlock 0)
    parser
    [ Task part1 374
    , AssertExample "expand 10" 1030 (distances 10)
    , AssertExample "expand 100" 8410 (distances 100)
    , TaskBlind part2
    ]
