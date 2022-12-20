{-# LANGUAGE Strict #-}

module Y2022.Day20 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser :: Parser Text ()
parser = error "parser"

addMod :: Int -> Int -> Int -> Int
addMod l a b = (a + b) `mod` l

withIndices :: [Int] -> [(Int, Int)]
withIndices = flip zip [0 ..]

unIndices :: [(Int, Int)] -> [Int]
unIndices = map fst

mix :: Int -> [Int] -> [Int]
mix times ns = unIndices $ iterateNL times go $ withIndices ns
  where
    go x = foldl' (flip mix1) x [0 .. l - 1]
    l = length ns

mix1 :: Int -> [(Int, Int)] -> [(Int, Int)]
mix1 n ns = ns2
  where
    l = length ns
    i = fromJustE "mix1" $ findIndex ((== n) . snd) ns
    (v@(vv, _), ns1) = sremove i ns
    i' = addMod (length ns1) i vv
    ns2 = sinsert i' v ns1

coordinates ns = sum $ map get [1000, 2000, 3000]
  where
    i0 = fromJustE "part1" $ elemIndex 0 ns
    l = length ns
    get i = ns !! ((i0 + i) `mod` l)

part1 = coordinates . mix 1

assert1 = unIndices . mix1 0 . withIndices

part2 = coordinates . mix 10 . map (811589153 *)

tasks =
  Tasks
    2022
    20
    (CodeBlock 0)
    (linesP &** integerP)
    [ AssertExample "mix 1" [2, 1, -3, 3, -2, 0, 4] assert1
    , Task part1 3
    , Task part2 1623178306
    ]
