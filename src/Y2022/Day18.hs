module Y2022.Day18 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type Cube = [Int]

parser :: Parser Text [Cube]
parser = linesP &** tsplitP "," &** integerP

adjanced :: Cube -> [Cube]
adjanced a =
  [sset i (f $ a !! i) a | i <- [0 .. length a - 1], f <- [succ, pred]]

freeSides :: [Cube] -> Int
freeSides cubes =
  length $ filter (not . (`elem` cubes)) $ concatMap adjanced cubes

tasks =
  Tasks
    2022
    18
    (CodeBlock 0)
    parser
    [ Assert "two cube sides" 10 $ freeSides [[0, 0, 0], [0, 0, 1]]
    , Task freeSides 64
    ]
