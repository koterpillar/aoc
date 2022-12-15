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
parser =
  linesP &** wordsP &*
  pureP
    (map (terase "x=" . terase "y=" . terase "," . terase ":") .
     filter (Text.isInfixOf "=")) &*
  pps &*
  (pp &= pp)
  where
    pps = pureP $ \[x1, y1, x2, y2] -> ((x1, y1), (x2, y2))
    pp = uncurry Position2 <$> (integerP &= integerP)

empties :: Position2 -> Position2 -> Set Position2
empties sensor@(Position2 sx sy) beacon =
  Set.fromList
    [ Position2 x y
    | x <- [xmin .. xmax]
    , y <- [ymin .. ymax]
    , manhattanDistance sensor (Position2 x y) <= d
    , Position2 x y /= beacon
    ]
  where
    d = manhattanDistance sensor beacon
    xmin = sx - d
    xmax = sx + d
    ymin = sy - d
    ymax = sy + d

part1 input =
  ttrace (displayG g) $ length $ Set.filter (\(Position2 _ y) -> y == 10) es
  where
    g = mkGrid $ traceShowId input
    es =
      Set.unions $ map (traceShowF length . uncurry empties . traceShowId) input

tasks = Tasks 2022 15 (CodeBlock 0) parser [Task part1 26]
