module Y2024.Day16
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

data Item
  = Wall
  | Start
  | End
  deriving (Ord, Eq, Show, Enum, Bounded)

instance GridItem Item where
  showInGrid Wall  = '#'
  showInGrid Start = 'S'
  showInGrid End   = 'E'

type Grid = Grid2 Item

parser :: Parser Text Grid
parser = charGridP

data Deer =
  Deer Position2 Direction4
  deriving (Ord, Eq, Show)

instance Hashable Deer where
  hashWithSalt s (Deer p d) = hashWithSalt s (p, d)

bestPath :: Grid -> Maybe [Deer]
bestPath g = (start :) <$> aStarGoal (moves g) score1 remaining start
  where
    start = Deer deerStart E
    deerStart = fromJustE "deerStart" $ findTuple (== Start) $ Map.toList g
    deerEnd = fromJustE "deerEnd" $ findTuple (== End) $ Map.toList g
    remaining (Deer p _) = manhattanDistance p deerEnd

moves :: Grid -> Deer -> [Deer]
moves g (Deer p d) =
  turns ++ [Deer (walk d p) d | Map.lookup (walk d p) g /= Just Wall]
  where
    turns = [Deer p $ turnLeft d, Deer p $ turnRight d]

score1 :: Deer -> Deer -> Int
score1 (Deer p1 d1) (Deer p2 d2)
  | d1 == d2 = 1
  | otherwise = 1000

score :: [Deer] -> Int
score = sum . zipWithTail score1

part1 :: Grid -> Int
part1 = score . fromJustE "no best path" . bestPath

tasks =
  Tasks
    2024
    16
    (CodeBlock 0)
    parser
    [task part1 7036 & taskPart 1, task part1 11048 & taskScraper (CodeBlock 2)]
