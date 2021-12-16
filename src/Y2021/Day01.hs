module Y2021.Day01 where

import           AOC
import           Utils

part1 = countTrue . zipWithTail (<)

take' n xs =
  let result = take n xs
   in if length result == n
        then pure result
        else mempty

slidingSum n = map sum . (=<<) (take' n) . tails

part2 = countTrue . zipWithTail (<) . slidingSum 3

tasks = Tasks 2021 1 (linesP &** integerP) [Task part1 7, Task part2 5]
