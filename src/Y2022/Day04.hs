module Y2022.Day04
  ( tasks
  ) where

import           AOC
import           Utils

type Range = (Int, Int)

parser :: Parser Text [(Range, Range)]
parser = linesP &** tsplitP "," &* rangeP &+ rangeP

rangeP :: Parser Text Range
rangeP = tsplitP "-" &* integerP &+ integerP

subsumes :: Range -> Range -> Bool
subsumes (a, b) (c, d) =
  inRange a b c && inRange a b d || inRange c d a && inRange c d b

overlaps :: Range -> Range -> Bool
overlaps (a, b) (c, d) =
  inRange a b c || inRange a b d || inRange c d a || inRange c d b

part1 = countIf (uncurry subsumes)

part2 = countIf (uncurry overlaps)

tasks = Tasks 2022 4 (CodeBlock 0) parser [Task part1 2, Task part2 4]
