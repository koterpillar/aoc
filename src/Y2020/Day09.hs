module Y2020.Day09 where

import           AOC
import           Utils

isValid :: [Int] -> Int -> Bool
isValid [] _ = False
isValid (x1:xs) y =
  let x2 = y - x1
   in x2 /= x1 && x2 `elem` xs || isValid xs y

findInvalid :: [Int] -> Int
findInvalid xs = findInvalid' (preambleLength xs) xs

preambleLength xs
  | length xs < 25 = 5
  | otherwise = 25

findInvalid' l xs =
  if not $ isValid preamble x
    then x
    else findInvalid' l $ tail xs
  where
    (preamble, x:_) = splitAt l xs

tasks = Tasks 2020 9 (CodeBlock 0) (linesP &** integerP) [Task findInvalid 127]
