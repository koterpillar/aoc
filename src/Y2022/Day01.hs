module Y2022.Day01
  ( tasks
  ) where

import           AOC
import           Utils

parser :: Parser Text [[Int]]
parser = lineGroupsP &** traverseP integerP

part1 :: [[Int]] -> Int
part1 = sum . maximumOn sum

part2 :: [[Int]] -> Int
part2 = sum . join . take 3 . sortOn (negate . sum)

tasks = Tasks 2022 1 (CodeBlock 0) parser [Task part1 24000, Task part2 45000]
