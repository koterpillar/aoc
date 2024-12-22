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

part1 :: [Int] -> Int
part1 = sum . map (iterateNL 2000 nextSecret)

tasks = Tasks 2024 22 (CodeBlock 1) parser [task part1 37327623 & taskPart 1]
