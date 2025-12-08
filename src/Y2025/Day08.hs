module Y2025.Day08 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

type Position3 = (Int, Int, Int)

distSq :: Position3 -> Position3 -> Int
distSq (x1, y1, z1) (x2, y2, z2) =
    (x2 - x1) * (x2 - x1)
        + (y2 - y1) * (y2 - y1)
        + (z2 - z1) * (z2 - z1)

parser :: Parser Text [Position3]
parser = linesP &** tsplitP "," &* ap3P (,,) integerP integerP integerP

initClusters :: [Position3] -> Map Position3 Int
initClusters pts = Map.fromList $ zip pts [0 ..]

pairs :: [Position3] -> [(Position3, Position3)]
pairs input = sortOn (\(pa, pb) -> distSq pa pb) [(p1, p2) | (p1 : t1) <- tails input, p2 <- t1]

part1 :: Int -> [Position3] -> Int
part1 cnt input = product $ take 3 $ reverse $ sort $ toList $ mapFromListCount $ toList $ foldr connect (initClusters input) $ take cnt $ pairs input

connect :: (Position3, Position3) -> Map Position3 Int -> Map Position3 Int
connect (pa, pb) m =
    let next = Map.size m
     in case (Map.lookup pa m, Map.lookup pb m) of
            (Nothing, Nothing) -> Map.insert pa next $ Map.insert pb next m
            (Just ca, Nothing) -> Map.insert pb ca m
            (Nothing, Just cb) -> Map.insert pa cb m
            (Just ca, Just cb) -> Map.map (\x -> if x == cb then ca else x) m

part2 :: [Position3] -> Int
part2 input = go (initClusters input) $ pairs input
  where
    go m [] = error "out of connections"
    go m ((pa, pb) : t) = let m' = connect (pa, pb) m in if done m' then r pa pb else go m' t
    done m = Set.size (Set.fromList $ Map.elems m) == 1
    r (xa, _, _) (xb, _, _) = xa * xb

tasks =
    Tasks
        2025
        8
        (CodeBlock 0)
        parser
        [ AssertExample "part1" 40 (part1 10)
        , taskBlind (part1 1000) & taskPart 1
        , task part2 25272 & taskPart 2
        ]
