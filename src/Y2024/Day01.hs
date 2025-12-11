module Y2024.Day01
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser :: Parser Text ([Int], [Int])
parser = fmap unzip $ linesP &** (wordsP &* (integerP &+ integerP))

part1 (as, bs) = sum $ zipWith (abs .: (-)) as' bs'
  where
    as' = sort as
    bs' = sort bs

part2 :: ([Int], [Int]) -> Int
part2 (as, bs) = sum $ map similarity as
  where
    bs' = mapFromListCount bs
    similarity a = a * fromMaybe 0 (Map.lookup a bs')

tasks =
  Tasks
    (AOC 2024 1)
    (CodeBlock 0)
    parser
    [task part1 11 & taskPart 1, task part2 31 & taskPart 2]
