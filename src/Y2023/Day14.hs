module Y2023.Day14 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Cycle
import           Grid
import           Utils

data Rock
  = O
  | X
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Rock where
  showInGrid O = 'O'
  showInGrid X = '#'

type Grid = Grid2 Rock

parser :: Parser Text Grid
parser = charGridP

roll :: Direction4 -> Grid -> Grid
roll d g = gx <> osNew
  where
    gx = Map.filter (== X) g
    os = [p | (p, O) <- Map.toList g]
    stops = mapFromListCount $ map (findStop d gx) os
    osNew =
      Map.fromList
        [(walkN i d' p, O) | (p, n) <- Map.toList stops, i <- [1 .. n]]
    d' = reverse4 d

findStop :: Direction4 -> Grid -> Position2 -> Position2
findStop d g p =
  fromJustE "findStop" $
  find (\p' -> not (insideBounds b p') || Map.member p' g) $ iterate (walk d) p
  where
    b = boundsG g

northLoad :: Grid -> Int
northLoad g = sum [ymax - y + 1 | (Position2 _ y, O) <- Map.toList g]
  where
    (_, Position2 _ ymax) = boundsG g

part1 = northLoad . roll N

rollCycle = roll E . roll S . roll W . roll N

part2 = northLoad . cycleElement 1000000000 . cycleFind rollCycle

tasks = Tasks 2023 14 (CodeBlock 0) parser [Task part1 136, Task part2 64]
