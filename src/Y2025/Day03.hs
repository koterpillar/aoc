module Y2025.Day03 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

parser :: Parser Text [[Int]]
parser = linesP &** charactersP &** (pureP Text.singleton &* integerP)

calc :: [Int] -> Int
calc = foldr (\x acc -> acc * 10 + x) 0 . reverse

findBatt :: Int -> [Int] -> [Int]
findBatt 0 _ = []
findBatt n xs = md : findBatt (pred n) rest
  where
    md = maximum $ dropEnd (pred n) xs
    rest = drop 1 $ dropWhile (/= md) xs

go :: Int -> [[Int]] -> Int
go n = sum . map (calc . findBatt n)

part1 = go 2

part2 = go 12

tasks =
    Tasks
        (AOC 2025 3)
        (CodeBlock 0)
        parser
        [ task part1 357 & taskPart 1
        , task part2 3121910778619 & taskPart 2
        ]
