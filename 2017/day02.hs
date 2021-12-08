module Day02 where

import Control.Applicative

lineChecksum :: [Int] -> Int
lineChecksum lst = maximum lst - minimum lst

checksum :: [[Int]] -> Int
checksum = sum . map lineChecksum

parseLines :: String -> [[Int]]
parseLines = map (map read . words) . lines

divideDivisible :: [Int] -> Int
divideDivisible lst =
  fst $
  head $
  filter (\(_, m) -> m == 0) $
  map (uncurry divMod) $ filter (uncurry (/=)) $ liftA2 (,) lst lst
