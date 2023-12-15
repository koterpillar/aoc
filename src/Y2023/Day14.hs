module Y2023.Day14 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
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

rollN :: Grid -> Grid
rollN g = gx <> osNew
  where
    gx = Map.filter (== X) g
    os = [p | (p, O) <- Map.toList g]
    stops = mapFromListCount $ map (findStopN gx) os
    osNew =
      Map.fromList
        [(walkN i S p, O) | (p, n) <- Map.toList stops, i <- [1 .. n]]

findStopN :: Grid -> Position2 -> Position2
findStopN g p =
  if pY p <= 0 || Map.member p' g
    then p'
    else findStopN g p'
  where
    p' = walk N p

northLoad :: Grid -> Int
northLoad g = sum [ymax - y + 1 | (Position2 _ y, O) <- Map.toList g]
  where
    (_, Position2 _ ymax) = boundsG g

part1 = northLoad . rollN

tasks = Tasks 2023 14 (CodeBlock 0) parser [Task part1 136]
