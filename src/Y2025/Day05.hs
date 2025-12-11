module Y2025.Day05 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import qualified Ranges
import Utils

type Input = (Ranges.Ranges, [Int])

parser :: Parser Text Input
parser = tsplitP "\n\n" &* (fmap mkRanges (linesP &** (tsplitP "-" &* ap2P (,) integerP integerP)) &+ (linesP &** integerP))

mkRanges :: [(Int, Int)] -> Ranges.Ranges
mkRanges = Ranges.fromList . map (second succ)

part1 :: Input -> Int
part1 (rs, is) = countIf (flip Ranges.member rs) is

part2 :: Input -> Int
part2 = Ranges.length . fst

tasks =
    Tasks
        (AOC 2025 5)
        (CodeBlock 0)
        parser
        [ task part1 3 & taskPart 1
        , task part2 14 & taskPart 2
        ]
