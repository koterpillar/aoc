module Y2022.Day10 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Instruction
  = Noop
  | Addx Int
  deriving (Ord, Eq, Show)

instrP :: Parser Text Instruction
instrP =
  wordsP &* (Noop <$ requireP ["noop"]) &|
  pairPWith (const Addx) (requireP "addx") integerP

data CPU =
  CPU
    { cX     :: Int
    , cCycle :: Int
    }
  deriving (Ord, Eq, Show)

initCPU :: CPU
initCPU = CPU 1 0

apply :: Instruction -> CPU -> CPU
apply Noop c     = c {cCycle = succ (cCycle c)}
apply (Addx x) c = c {cX = cX c + x, cCycle = cCycle c + 2}

applyMany :: [Instruction] -> CPU
applyMany = foldl' (flip apply) initCPU

part1test :: [Instruction] -> Int
part1test = cX . applyMany

applyTrail :: [Instruction] -> [CPU]
applyTrail = go initCPU
  where
    go _ [] = []
    go c (i:is) =
      let c' = apply i c
       in c' : go c' is

checkpoints :: [Int]
checkpoints = [20, 60, 100, 140, 180, 220]

part1 is =
  let trail = applyTrail is
   in sum $ map (signalStrength trail) checkpoints

signalStrength :: [CPU] -> Int -> Int
signalStrength trail pc = traceShowF (\r -> (pc, r)) (cX r * pc)
  where
    r = go trail
    go (c1:c2:cs)
      | cCycle c2 >= pc = c1
      | otherwise = go (c2 : cs)
    go _ = error "pc too large"

tasks =
  Tasks
    2022
    10
    (CodeBlock 1)
    (linesP &** instrP)
    [TaskScraper (CodeBlock 0) part1test (-1), Task part1 13140]
