module Y2023.Day09 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser = linesP &** integersSpaceP

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

predict :: [Int] -> Int
predict xs
  | all (== 0) xs = 0
  | otherwise = last xs + y'
  where
    ys = diff xs
    y' = predict ys

part1 = sum . map predict

part2 = part1 . map reverse

tasks = Tasks 2023 9 (CodeBlock 0) parser [Task part1 114, Task part2 2]
