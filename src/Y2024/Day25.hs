module Y2024.Day25
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

parser :: Parser Text [Grid]
parser = lineGroupsP &** (pureP Text.unlines &* dotGridP)

part1 :: [Grid] -> Int
part1 gs =
  length $ do
    g1:r <- tails gs
    g2 <- r
    guard $ Map.disjoint g1 g2

tasks = Tasks (AOC 2024 25) (CodeBlock 0) parser [task part1 3 & taskPart 1]
