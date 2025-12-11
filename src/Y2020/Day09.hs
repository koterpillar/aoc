module Y2020.Day09
  ( tasks
  ) where

import           AOC
import           Utils

isValid :: [Int] -> Int -> Bool
isValid [] _ = False
isValid (x1:xs) y =
  let x2 = y - x1
   in x2 /= x1 && x2 `elem` xs || isValid xs y

findInvalid :: [Int] -> Int
findInvalid xs = findInvalid' (preambleLength xs) xs

preambleLength :: [Int] -> Int
preambleLength xs
  | length xs < 25 = 5
  | otherwise = 25

findInvalid' :: Int -> [Int] -> Int
findInvalid' l xs =
  if not $ isValid preamble x
    then x
    else findInvalid' l $ tail xs
  where
    (preamble, x:_) = splitAt l xs

findSum :: [Int] -> Int
findSum xs = minimum rng + maximum rng
  where
    rng = findSum' invalid [] xs
    invalid = findInvalid xs

findSum' :: Int -> [Int] -> [Int] -> [Int]
findSum' target xs rest
  | sum xs == target = xs
  | sum xs > target = findSum' target (tail xs) rest
  | null rest = error "exhausted"
  | otherwise = findSum' target (xs ++ [head rest]) (tail rest)

tasks =
  Tasks
    (AOC 2020 9)
    (CodeBlock 0)
    (linesP &** integerP)
    [Task findInvalid 127, Task findSum 62]
