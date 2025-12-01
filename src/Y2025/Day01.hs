module Y2025.Day01 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

parser :: Parser Text [Int]
parser = linesP &** (tspanP isUpper &* fmap (uncurry (*)) (signP &= integerP))
  where
    signP :: Parser Text Int
    signP = choiceP [("L", -1), ("R", 1)]

part1 :: [Int] -> Int
part1 = go 0 50
  where
    go r _ [] = r
    go r v (x : xs) =
        let v' = (v + x) `mod` 100
            r' = if v' == 0 then r + 1 else r
         in go r' v' xs

part2 = part1 . go
  where
    go [] = []
    go (x : xs) | x > 0 = replicate x 1 ++ go xs
                | x < 0 = replicate (abs x) (-1) ++ go xs
                | otherwise = go xs

tasks =
    Tasks
        2025
        1
        (CodeBlock 0)
        parser
        [ task part1 3 & taskPart 1
        , task part2 6 & taskPart 2
        ]
