module Y2022.Day06
  ( tasks
  ) where

import qualified Data.Set as Set

import           AOC
import           Utils

part1 :: Ord a => [a] -> Int
part1 = go 4 0

part2 :: Ord a => [a] -> Int
part2 = go 14 0

go :: Ord a => Int -> Int -> [a] -> Int
go requiredDifferent idx lst =
  if length (Set.fromList $ take requiredDifferent lst) == requiredDifferent
    then idx + requiredDifferent
    else go requiredDifferent (idx + 1) (tail lst)

tasks = Tasks 2022 6 (InlineCode 3) charactersP [Task part1 11, Task part2 26]
