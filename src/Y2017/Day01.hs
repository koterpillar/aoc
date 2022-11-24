module Y2017.Day01 where

import           AOC
import           Utils

import           Data.Char

go' :: Eq a => [a] -> [a]
go' (x1:x2:xs)
  | x1 == x2 = x1 : go' (x2 : xs)
  | otherwise = go' (x2 : xs)
go' _ = []

go :: String -> Int
go lst = sumdigits $ go' (lst ++ [head lst])

sumdigits = sum . map c2i
  where
    c2i c = ord c - ord '0'

go2 lst = 2 * sumdigits (map fst $ filter (uncurry (==)) $ splitintwo lst)

splitintwo lst = zip (take half lst) (drop half lst)
  where
    half = length lst `div` 2

tasks =
  Tasks
    2017
    1
    (Inline "91212129")
    (linesP &* singleP &* charactersP)
    [Task go 9, Task go2 6]
