module Y2020.Day10 where

import qualified Data.Map.Strict as Map

import           AOC
import           Memo
import           Utils

oneAndThree :: [Int] -> Int
oneAndThree adapters = countIf (== 1) stepups * countIf (== 3) stepups
  where
    stepups = zipWithTail subtract joltages
    joltages = 0 : sort adapters ++ [maximum adapters + 3]

countChains :: [Int] -> Int
countChains allAdapters = go (Map.singleton 0 1) $ sort allAdapters
  where
    target = maximum allAdapters + 3
    go :: Map Int Int -> [Int] -> Int
    go v [] = sum $ Map.filterWithKey (closeTo target) v
    go v (x:xs) = go (v1 `mapSum` Map.singleton x r) xs
      where
        v1 = Map.filterWithKey (closeTo x) v
        r = sum v1
    closeTo x k _ = x - k <= 3

tasks =
  Tasks
    2020
    10
    (CodeBlock 1)
    (linesP &** integerP)
    [Task oneAndThree 220, Task countChains 19208]
