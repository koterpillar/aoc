module Y2025.Day06 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

parser :: Parser Text [[Char]]
parser = linesP &** charactersP

type Op = Int -> Int -> Int

mkOp "+" = (+)
mkOp "*" = (*)

calc :: Op -> [[Char]] -> Int
calc op ns = foldr1 op $ map read ns

calc1 :: [[Char]] -> Int
calc1 (op : ns) = calc (mkOp op) ns

part1 :: [[Char]] -> Int
part1 = sum . map calc1 . map reverse . transpose . map words

calc2 :: [[Char]] -> Int
calc2 ((opc : n1) : ns) = calc (mkOp [opc]) $ map reverse (n1 : ns)

part2 :: [[Char]] -> Int
part2 = sum . map calc2 . splitOn [""] . map trim . transpose . reverse

tasks =
    Tasks
        2025
        6
        (CodeBlock 0)
        parser
        [ task part1 4277556 & taskPart 1
        , task part2 3263827 & taskPart 2
        ]
