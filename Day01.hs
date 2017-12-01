module Day01 where

import Data.Char

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

go2 lst = 2 * (sumdigits $ map fst $ filter (uncurry (==)) $ splitintwo lst)

splitintwo lst = zip (take half lst) (drop half lst)
  where
    half = length lst `div` 2
