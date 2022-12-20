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

mix :: [Int] -> [Int]
mix ns = unIndices $ foldl' (flip mix1) (withIndices ns) [0 .. l - 1]
  where
    l = length ns

mix1 :: Int -> [(Int, Int)] -> [(Int, Int)]
mix1 n ns = ns2
  where
    l = length ns
    i = fromJustE "mix1" $ findIndex ((== n) . snd) ns
    (v@(vv, _), ns1) = sremove i ns
    i' = addMod (length ns1) i vv
    ns2 = sinsert i' v ns1

part1 ns = sum $ map get [1000, 2000, 3000]
  where
    ns1 = traceF (prependShow "ns1") $ mix $ traceF (prependShow "input") ns
    l = length ns
    i0 = fromJustE "part1" $ elemIndex 0 ns1
    get i = ns1 !! ((i0 + i) `mod` l)

assert1 = unIndices . mix1 0 . withIndices

tasks =
  Tasks
    2022
    20
    (CodeBlock 0)
    (linesP &** integerP)
    [AssertExample "mix 1" [2, 1, -3, 3, -2, 0, 4] assert1, Task part1 3]
