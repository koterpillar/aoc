module Y2025.Day11 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Memo
import Utils

type Input = Map Text [Text]

parser :: Parser Text Input
parser = fmap Map.fromList $ linesP &** tsplitP ": " &* (idP &+ wordsP)

paths :: (Hashable a, Ord a, Show a) => a -> a -> Map a [a] -> Int
paths from to graph = traceShowF (from, to,) $ stateMemo go ($ from)
  where
    go rgo current
        | current == to = pure 1
        | otherwise = fmap sum $ traverse rgo $ Map.findWithDefault [] current graph

part1 :: Input -> Int
part1 = paths "you" "out"

part2 :: Input -> Int
part2 g = a1 * b1 * c1 + a2 * b2 * c2
  where
    a1 = paths "svr" "fft" g
    a2 = paths "svr" "dac" g
    b1 = paths "fft" "dac" g
    b2 = paths "dac" "fft" g
    c1 = paths "dac" "out" g
    c2 = paths "fft" "out" g

tasks =
    Tasks
        2025
        11
        (CodeBlock 0)
        parser
        [ task part1 5 & taskPart 1
        , task part2 2 & taskPart 2 & taskScraper (CodeBlock 1)
        ]
