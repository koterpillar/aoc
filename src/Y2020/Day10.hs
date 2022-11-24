module Y2020.Day10 where

import           AOC
import           Utils

oneAndThree :: [Int] -> Int
oneAndThree adapters = countIf (== 1) stepups * countIf (== 3) stepups
  where
    stepups = zipWith (-) (tail joltages) joltages
    joltages = 0 : sort adapters ++ [maximum adapters + 3]

tasks = Tasks 2020 10 (CodeBlock 1) (linesP &** integerP) [Task oneAndThree 220]
