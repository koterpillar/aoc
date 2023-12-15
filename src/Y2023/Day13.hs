module Y2023.Day13 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

transposeG :: Grid -> Grid
transposeG = Map.mapKeys (\(Position2 x y) -> Position2 y x)

parser :: Parser Text [Grid]
parser = lineGroupsP &** (pureP Text.unlines &* dotGridP)

xrange :: Grid -> [Int]
xrange g = [succ xmin .. xmax]
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g

findReflectionV :: Grid -> Maybe Int
findReflectionV g = find (null . smudgesV g) (xrange g)

smudgesV :: Grid -> Int -> [Position2]
smudgesV g x = filter (flippedMissing x) (Map.keys g)
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g
    flippedMissing xc (Position2 x y) =
      xf >= xmin && xf <= xmax && Map.notMember (Position2 xf y) g
      where
        xf = 2 * xc - 1 - x

findSmudgeV :: Grid -> Maybe Int
findSmudgeV g = find (single . smudgesV g) (xrange g)

single :: [a] -> Bool
single [_] = True
single _   = False

reflectionValue :: (Grid -> Maybe Int) -> Grid -> Int
reflectionValue f g =
  case (v, h) of
    (Just v', Nothing) -> v'
    (Nothing, Just h') -> h' * 100
    e ->
      terror $ "Unexpected reflections: " <> tshow e <> " in \n" <> displayG g
  where
    v = f g
    h = f $ transposeG g

part1 :: [Grid] -> Int
part1 = sum . map (reflectionValue findReflectionV)

part2 :: [Grid] -> Int
part2 = sum . map (reflectionValue findSmudgeV)

tasks = Tasks 2023 13 (CodeBlock 0) parser [Task part1 405, Task part2 400]
