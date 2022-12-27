module Y2020.Day10 where

import           AOC
import           Utils

oneAndThree :: [Int] -> Int
oneAndThree adapters = countIf (== 1) stepups * countIf (== 3) stepups
  where
    stepups = zipWith (-) (tail joltages) joltages
    joltages = 0 : sort adapters ++ [maximum adapters + 3]

countChains :: [Int] -> Int
countChains allAdapters = memoFix2 go 0 $ sort allAdapters
  where
    target = maximum allAdapters + 3
    go :: (Int -> [Int] -> Int) -> Int -> [Int] -> Int
    go _ v []
      | target - v > 3 = 0
      | otherwise = 1
    go igo v (x:xs)
      | x - v > 3 = 0
      | otherwise = igo v xs + igo x xs

tasks =
  Tasks
    2020
    10
    (CodeBlock 1)
    (linesP &** integerP)
    [task oneAndThree 220, task countChains 19208]
