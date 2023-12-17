module Y2022.Day14 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data CI
  = Rock
  | Sand
  deriving (Ord, Eq, Show)

instance GridItem CI where
  showInGrid Rock = '#'
  showInGrid Sand = 'o'

type Grid = Grid2 CI

type Input = [[Position2]]

parser :: Parser Text Input
parser = linesP &** pp
  where
    pp = tsplitP " -> " &** position2P

mkGrid :: Input -> Grid
mkGrid = Map.unions . map go
  where
    go = Map.fromList . map (, Rock) . concatMap (uncurry go1) . zipTail
    go1 p1@(Position2 x1 y1) (Position2 x2 y2)
      | y1 > y2 = p1 : go1 (Position2 x1 (y1 - 1)) (Position2 x2 y2)
      | y1 < y2 = p1 : go1 (Position2 x1 (y1 + 1)) (Position2 x2 y2)
      | x1 < x2 = p1 : go1 (Position2 (x1 + 1) y1) (Position2 x2 y2)
      | x1 > x2 = p1 : go1 (Position2 (x1 - 1) y1) (Position2 x2 y2)
      | otherwise = [p1]

dropSand :: Maybe Int -> Position2 -> Grid -> Grid
dropSand floor p g =
  if pY p > maxY
    then g
    else case nextPoint of
           Just p' -> dropSand floor p' g
           Nothing -> Map.insert p Sand g
  where
    (_, Position2 _ maxY') = boundsG g
    maxY = fromMaybe maxY' floor
    nextPoint = find free [walk S p, walk SW p, walk SE p]
    free p
      | Just (pY p) == floor = False
      | otherwise = isNothing (Map.lookup p g)

sandInit :: Position2
sandInit = Position2 500 0

dropAllSand :: Maybe Int -> Grid -> Grid
dropAllSand floor = iterateSettleL $ dropSand floor sandInit

countSand :: Maybe Int -> Grid -> Int
countSand y = countElem Sand . dropAllSand y

part1 = countSand Nothing . mkGrid

part2 input =
  let g = mkGrid input
   in countSand (Just $ floorY g) g

floorY :: Grid2 a -> Int
floorY g =
  let (_, Position2 _ maxY) = boundsG g
   in maxY + 2

tasks = Tasks 2022 14 (CodeBlock 0) parser [Task part1 24, Task part2 93]
