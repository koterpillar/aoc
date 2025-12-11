module Y2021.Day01
  ( tasks
  ) where

import           AOC
import           Utils

part1 = countTrue . zipWithTail (<)

chunks n as = do
  t <- tails as
  let chunk = take n t
  guard $ length chunk == n
  pure chunk

slidingSum n = map sum . chunks n

part2 = countTrue . zipWithTail (<) . slidingSum 3

tasks =
  Tasks (AOC 2021 1) (CodeBlock 0) (linesP &** integerP) [Task part1 7, Task part2 5]
