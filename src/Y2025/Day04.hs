module Y2025.Day04 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Grid
import Utils

parser :: Parser Text (Grid2 ())
parser = charGridP' $ choiceP [('@', ())]

removable :: Grid2 () -> [Position2]
removable g = filter go $ Map.keys g
  where
    go pos
        | adj pos < 4 = True
        | otherwise = False
    adj p = countIf (`Map.member` g) (adjacent8 p)

part1 :: Grid2 () -> Int
part1 = length . removable

part2 :: Grid2 () -> Int
part2 g
    | null r = 0
    | otherwise = length r + part2 g'
  where
    r = removable g
    g' = foldr Map.delete g r

tasks =
    Tasks
        2025
        4
        (CodeBlock 0)
        parser
        [ task part1 13 & taskPart 1
        , task part2 43 & taskPart 2
        ]
