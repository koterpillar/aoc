module Y2024.Day07
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Calibration = Calibration
  { cResult  :: Int
  , cNumbers :: [Int]
  } deriving (Ord, Eq, Show)

parser :: Parser Text [Calibration]
parser = linesP &** (tsplitP ": " &* ap2P Calibration integerP integersSpaceP)

type Operator = Int -> Int -> Int

solvable :: [Operator] -> Calibration -> Bool
solvable _ (Calibration result []) = error "Empty calibration"
solvable _ (Calibration result [x]) = x == result
solvable operators (Calibration result (x1:x2:xs))
  | x1 > result = False
  | otherwise =
    any
      (\op -> solvable operators $ Calibration result (op x1 x2 : xs))
      operators

resultsOfSolvable :: [Operator] -> [Calibration] -> Int
resultsOfSolvable operators = sum . map cResult . filter (solvable operators)

part1 :: [Calibration] -> Int
part1 = resultsOfSolvable [(+), (*)]

appendDigits :: Int -> Int -> Int
appendDigits a b = read $ show a ++ show b

part2 :: [Calibration] -> Int
part2 = resultsOfSolvable [(+), (*), appendDigits]

tasks =
  Tasks
    2024
    7
    (CodeBlock 0)
    parser
    [task part1 3749 & taskPart 1, task part2 11387 & taskPart 2]
