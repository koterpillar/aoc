module Y2025.Day09 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Grid
import Utils

parser :: Parser Text [Position2]
parser = linesP &** (tsplitP "," &* ap2P Position2 integerP integerP)

pairs input = [(p1, p2) | (p1 : t1) <- tails input, p2 <- t1]

rectangleSize :: Position2 -> Position2 -> Int
rectangleSize (Position2 x1 y1) (Position2 x2 y2) = succ (abs $ x2 - x1) * succ (abs $ y2 - y1)

maxRectangle :: [(Position2, Position2)] -> Int
maxRectangle = maximum . map (uncurry rectangleSize)

part1 :: [Position2] -> Int
part1 = maxRectangle . pairs

inside :: Position2 -> Position2 -> [Position2]
inside (Position2 x1 y1) (Position2 x2 y2) =
    [Position2 ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)]
        ++ [ Position2 x y
           | x <- [min x1 x2 + 1 .. max x1 x2 - 1]
           , y <- [min y1 y2 + 1 .. max y1 y2 - 1]
           ]

line :: Position2 -> Position2 -> Set Position2
line (Position2 x1 y1) (Position2 x2 y2)
    | x1 == x2 = Set.fromList [Position2 x1 y | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = Set.fromList [Position2 x y1 | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = error "not a straight line"

allLines :: [Position2] -> Set Position2
allLines ps = Set.unions $ zipWithTail line ps'
  where
    ps' = last ps : ps

part2 :: [Position2] -> Int
part2 ps = maxRectangle $ filter (not . uncurry hasHoles) $ pairs ps
  where
    c = compressPoints ps
    pss = allLines $ map c ps
    hasHoles p1 p2 = any (`Set.member` pss) $ inside (c p1) (c p2)

tasks =
    Tasks
        2025
        9
        (CodeBlock 0)
        parser
        [ task part1 50 & taskPart 1
        , task part2 24 & taskPart 2
        ]
