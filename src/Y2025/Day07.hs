module Y2025.Day07 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Grid
import Utils

data Item = Src | V deriving (Eq, Ord, Show, Bounded, Enum)

type Grid = Grid2 Item

instance GridItem Item where
    showInGrid Src = 'S'
    showInGrid V = '^'

parser :: Parser Text Grid
parser = charGridP

type Split = Int -> (Int, Int)

step :: Split -> Grid -> Map Position2 Int -> Map Position2 Int
step split grid = Map.unionsWith (+) . map go . Map.toList
  where
    go (pos, w) =
        let pos' = walk S pos
         in case Map.lookup (walk S pos) grid of
                Just V -> let (w1, w2) = split w in Map.fromList [(walk W pos', w1), (walk E pos', w2)]
                Nothing -> Map.singleton pos' w

part :: Split -> Int -> Grid -> Int
part split start g = sum $ go $ Map.singleton (mapFindValueE "src" (== Src) g) start
  where
    maxY = pY (maximumOn pY $ Map.keys g)
    go st
        | pY (fst $ Map.findMin st) > maxY = st
        | otherwise = go $ step split g st

split1 :: Split
split1 n = (n, 1)

split2 :: Split
split2 n = (n, n)

part1 = part split1 0
part2 = part split2 1

tasks =
    Tasks
        (AOC 2025 7)
        (CodeBlock 0)
        parser
        [ task part1 21 & taskPart 1
        , task part2 40 & taskPart 2
        ]
