module Y2023.Day01 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser :: Parser Text [[Char]]
parser = linesP &** charactersP

decimal :: [Int] -> Int
decimal ns = head ns * 10 + last ns

part1 :: [[Char]] -> Int
part1 = sum . map (decimal . mapMaybe naiveConvert)

naiveConvert :: Char -> Maybe Int
naiveConvert c
  | isDigit c = Just $ ord c - ord '0'
  | otherwise = Nothing

extractChars :: String -> [Int]
extractChars = mapMaybe extractChar . join . map tails . inits

extractChar :: [Char] -> Maybe Int
extractChar "one"   = Just 1
extractChar "two"   = Just 2
extractChar "three" = Just 3
extractChar "four"  = Just 4
extractChar "five"  = Just 5
extractChar "six"   = Just 6
extractChar "seven" = Just 7
extractChar "eight" = Just 8
extractChar "nine"  = Just 9
extractChar [c]     = naiveConvert c
extractChar _       = Nothing

part2 :: [String] -> Int
part2 = sum . map (decimal . extractChars)

tasks =
  Tasks
    2023
    1
    (CodeBlock 0)
    parser
    [task part1 142, task part2 281 & taskScraper (CodeBlock 1)]
