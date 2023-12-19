{-# LANGUAGE Strict #-}

module Y2022.Day15 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Item
  = Sensor
  | Beacon
  | Empty
  deriving (Ord, Eq, Show)

instance GridItem Item where
  showInGrid Sensor = 'S'
  showInGrid Beacon = 'B'
  showInGrid Empty  = '#'

type Input = [(Position2, Position2)]

type Grid = Grid2 Item

mkGrid :: Input -> Grid
mkGrid = Map.fromList . concatMap (\(p1, p2) -> [(p1, Sensor), (p2, Beacon)])

parser :: Parser Text Input
parser = linesP &** tsplitP ":" &* (part &+ part)
  where
    part =
      pureP (terase "x=" . terase "y=" . terase "," . terase ":")
        &* wordsP
        &* pureP (takeEnd 2)
        &* ap2P Position2 integerP integerP

empties :: Int -> Position2 -> Position2 -> Set Position2
empties y sensor@(Position2 sx sy) beacon =
  Set.fromList
    [ p
    | x <- [xmin .. xmax]
    , let p = Position2 x y
    , manhattanDistance sensor p <= d
    , p /= beacon
    ]
  where
    d = manhattanDistance sensor beacon
    xmin = sx - d
    xmax = sx + d
    ymin = sy - d
    ymax = sy + d

isExample :: Input -> Bool
isExample input = fst (head input) == Position2 2 18

part1 input = length $ Map.filter (== Empty) g'
  where
    g = mkGrid input
    filterY =
      if isExample input
        then 10
        else 2000000
    es = Set.unions $ map (uncurry $ empties filterY) input
    g' = Map.union g $ Map.fromSet (const Empty) es

tuningFreq :: Position2 -> Int
tuningFreq (Position2 x y) = x * 4000000 + y

signs :: [Int -> Int -> Int]
signs = [(+), (-)]

borders :: Position2 -> Position2 -> [Position2]
borders sensor@(Position2 x y) beacon =
  [ Position2 (x `s1` o) (y `s2` (d - o))
  | o <- [0 .. d]
  , s1 <- signs
  , s2 <- signs
  ]
  where
    d = manhattanDistance sensor beacon + 1

part2 :: Input -> Int
part2 input =
  tuningFreq
    $ fromSingleE "filtered"
    $ nubOrd
    $ filter (\p -> all (invisibleTo p) input) bs
  where
    mx =
      if isExample input
        then 20
        else 4000000
    inRangeP (Position2 x y) = inRange 0 mx x && inRange 0 mx y
    bs = concatMap (filter inRangeP . uncurry borders) input
    invisibleTo p (sensor, beacon) =
      manhattanDistance p sensor > manhattanDistance beacon sensor

tasks = Tasks 2022 15 (CodeBlock 0) parser [Task part1 26, Task part2 56000011]
