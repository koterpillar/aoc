module Y2022.Day10 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Instruction
  = Noop
  | Addx Int
  deriving (Ord, Eq, Show)

instrP :: Parser Text Instruction
instrP = wordsP &* unconsBindP go
  where
    go "noop" = ap0P Noop
    go "addx" = ap1P Addx integerP

data CPU =
  CPU
    { cX       :: Int
    , cCycle   :: Int
    , cHistory :: Map Int Int
    }
  deriving (Ord, Eq, Show)

initCPU :: CPU
initCPU = CPU 1 1 Map.empty

tick :: CPU -> CPU
tick c = c {cCycle = pc', cHistory = Map.insert pc (cX c) (cHistory c)}
  where
    pc = cCycle c
    pc' = succ pc

apply :: Instruction -> CPU -> CPU
apply Noop c = tick c
apply (Addx x) c =
  let c1 = tick (tick c)
   in c1 {cX = cX c1 + x}

applyMany :: [Instruction] -> CPU
applyMany = foldl' (flip apply) initCPU

part1test :: [Instruction] -> Int
part1test = cX . applyMany

checkpoints :: [Int]
checkpoints = [20, 60, 100, 140, 180, 220]

part1 is =
  let cpu = applyMany is
   in sum $ map (signalStrength cpu) checkpoints

cAt :: Int -> CPU -> Int
cAt pc cpu = mapLookupE "pc not found" pc (cHistory cpu)

signalStrength :: CPU -> Int -> Int
signalStrength cpu pc = traceShowF (pc, ) (r * pc)
  where
    r = cAt pc cpu

part2 :: [Instruction] -> ()
part2 is = ttrace (displayG screen) ()
  where
    screen =
      Map.fromList [(sp2pt pc, ()) | pc <- screenpoints, pixelVisible pc cpu]
    cpu = traceShowId $ applyMany is

screenpoints :: [Int]
screenpoints = [1 .. 240]

sp2pt :: Int -> Position2
sp2pt i =
  let j = i - 1
   in Position2 (j `mod` 40) (j `div` 40)

pixelVisible :: Int -> CPU -> Bool
pixelVisible pc cpu = traceShowF (pc, spr, scan, ) $ abs (spr - scan) <= 1
  where
    spr = cAt pc cpu
    (Position2 scan _) = sp2pt pc

tasks =
  Tasks
    2022
    10
    (CodeBlock 1)
    (linesP &** instrP)
    [TaskScraper (CodeBlock 0) part1test (-1), Task part1 13140, Task part2 ()]
