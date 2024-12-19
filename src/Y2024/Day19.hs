module Y2024.Day19
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Memo
import           Utils

type Input = (Set Text, [Text])

parser :: Parser Text Input
parser = tsplitP "\n\n" &* ((Set.fromList <$> tsplitP ", ") &+ linesP)

allCreates :: Set Text -> Text -> Int
allCreates towels pattern = stateMemo go ($ pattern)
  where
    go _ "" = pure 1
    go rgo p =
      fmap sum
        $ traverse rgo
        $ catMaybes [Text.stripPrefix t p | t <- Set.toList towels]

mapTowels :: Set Text -> [Text] -> [Int]
mapTowels towels = map $ allCreates towels

part1 :: Input -> Int
part1 = uncurry $ countIf (/= 0) .: mapTowels

part2 :: Input -> Int
part2 = uncurry $ sum .: mapTowels

tasks =
  Tasks
    2024
    19
    (CodeBlock 0)
    parser
    [task part1 6 & taskPart 1, task part2 16 & taskPart 2]
