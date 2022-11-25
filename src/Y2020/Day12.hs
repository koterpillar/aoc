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
  (uncurry id) <$>
  tspanP isLetter &*
  (choiceP
     ([(tshow d, Move d) | d <- allDir4] ++
      [("L", RotateLeft), ("R", RotateRight), ("F", Forward)]) &=
   integerP)

data Ship =
  Ship
    { sPosition  :: Position2
    , sDirection :: Direction4
    }
  deriving (Eq, Show)

move :: Instruction -> Ship -> Ship
move (Move d i) (Ship p ds)      = Ship (walkN i d p) ds
move (RotateLeft i) (Ship p ds)  = Ship p (iterate turnLeft ds !! (i `div` 90))
move (RotateRight i) (Ship p ds) = Ship p (iterate turnRight ds !! (i `div` 90))
move (Forward i) (Ship p ds)     = Ship (walkN i ds p) ds

moves :: [Instruction] -> Ship -> Ship
moves [] s     = s
moves (x:xs) s = moves xs (move x s)

part1 instructions = manhattanDistance (sPosition start) (sPosition end)
  where
    start = Ship (Position2 0 0) E
    end = moves instructions start

tasks =
  Tasks 2020 12 (CodeBlock 0) (linesP &** parseInstruction) [Task part1 25]
