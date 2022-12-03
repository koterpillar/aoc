module Y2020.Day12 where

import           Data.Char

import           AOC
import           Grid
import           Utils

data Instruction
  = Move Direction4 Int
  | RotateLeft Int
  | RotateRight Int
  | Forward Int
  deriving (Eq, Show)

parseInstruction :: Parser Text Instruction
parseInstruction =
  uncurry id <$>
  tspanP isLetter &*
  choiceP
    ([(tshow d, Move d) | d <- allDir4] ++
     [("L", RotateLeft), ("R", RotateRight), ("F", Forward)]) &=
  integerP

data Ship =
  Ship
    { sPosition  :: Position2
    , sDirection :: Direction4
    }
  deriving (Eq, Show)

move :: Ship -> Instruction -> Ship
move (Ship p ds) (Move d i)      = Ship (walkN i d p) ds
move (Ship p ds) (RotateLeft i)  = Ship p (iterateN (i `div` 90) turnLeft ds)
move (Ship p ds) (RotateRight i) = Ship p (iterateN (i `div` 90) turnRight ds)
move (Ship p ds) (Forward i)     = Ship (walkN i ds p) ds

part1 instructions = manhattanDistance (sPosition start) (sPosition end)
  where
    start = Ship (Position2 0 0) E
    end = foldl' move start instructions

data Ship2 =
  Ship2
    { sPosition2 :: Position2
    , sWaypoint  :: Position2
    }
  deriving (Eq, Show)

move2 :: Ship2 -> Instruction -> Ship2
move2 (Ship2 p w) (Move d i) = Ship2 p (walkN i d w)
move2 (Ship2 p w) (Forward i) = Ship2 (p `pointPlus` (i `pointM` w)) w
move2 (Ship2 p w) (RotateLeft i) =
  Ship2 p $ iterateN (i `div` 90) pointRotateLeft w
move2 (Ship2 p w) (RotateRight i) =
  Ship2 p $ iterateN (i `div` 90) pointRotateRight w

part2 instructions = manhattanDistance (sPosition2 start) (sPosition2 end)
  where
    start = Ship2 (Position2 0 0) (Position2 10 (-1))
    end = foldl' move2 start instructions

tasks =
  Tasks
    2020
    12
    (CodeBlock 0)
    (linesP &** parseInstruction)
    [Task part1 25, Task part2 286]
