module Y2025.Day02 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

parser :: Parser Text [(Int, Int)]
parser = tsplitP "," &** (tsplitP "-" &* ap2P (,) integerP integerP)

sillyId :: Int -> Bool
sillyId i = even l && s1 == s2
  where
    s = show i
    l = length s
    s1 = take (l `div` 2) s
    s2 = drop (l `div` 2) s

part f = sum . filter f . join . map ((\(a, b) -> [a .. b]))

part1 = part sillyId

sillyId2 :: Int -> Bool
sillyId2 i = any go [1 .. l `div` 2]
  where
    s = show i
    l = length s
    go d = join (replicate (l `div` d) (take d s)) == s

part2 = part sillyId2

tasks =
    Tasks
        (AOC 2025 2)
        (CodeBlock 0)
        parser
        [ task part1 1227775554 & taskPart 1
        , task part2 4174379265 & taskPart 2
        ]
