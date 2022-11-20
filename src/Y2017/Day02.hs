module Y2017.Day02 where

import qualified Data.Text as Text

import           AOC
import           Utils

lineChecksum :: [Int] -> Int
lineChecksum lst = maximum lst - minimum lst

checksum :: [[Int]] -> Int
checksum = sum . map lineChecksum

divideDivisible :: [Int] -> Int
divideDivisible lst =
  fst $
  head $
  filter (\(_, m) -> m == 0) $
  map (uncurry divMod) $ filter (uncurry (/=)) $ liftA2 (,) lst lst

tasks =
  Tasks
    2017
    2
    (CodeBlock 0)
    (linesP &** (wordsP &** integerP))
    [Task checksum 18, TaskScraper (CodeBlock 1) (sum . map divideDivisible) 9]
