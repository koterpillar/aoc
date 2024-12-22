module Y2024.Day22
  ( tasks
  ) where

import           Control.Monad.State

import           Data.Bits

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

parser :: Parser Text [Int]
parser = linesP &** integerP

mix = xor

prune s = s `mod` 16777216

nextSecret :: Int -> Int
nextSecret s =
  flip execState s $ do
    a <- gets (* 64)
    modify $ mix a
    modify prune
    b <- gets (`div` 32)
    modify $ mix b
    modify prune
    c <- gets (* 2048)
    modify $ mix c
    modify prune

price :: Int -> Int
price s = s `mod` 10

secretSequence :: Int -> [Int]
secretSequence = iterateN 2000 nextSecret

part1 :: [Int] -> Int
part1 = sum . map (last . secretSequence)

earnings :: [Int] -> Map [Int] Int
earnings ss =
  Map.fromList $ do
    ps' <- reverse $ tails ss
    guard $ length ps' >= 5
    let ps = take 5 ps'
    let changes = zipWithTail subtract ps
    pure (changes, last ps)

part2 :: [Int] -> Int
part2 ss = maximum es
  where
    ess = map (earnings . map price . secretSequence) ss
    es = foldl1 (Map.unionWith (+)) ess

tasks =
  Tasks
    2024
    22
    (CodeBlock 1)
    parser
    [ task part1 37327623 & taskPart 1
    , task part2 23 & taskPart 2 & taskScraper (CodeBlock 5)
    ]
