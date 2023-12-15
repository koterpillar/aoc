module Y2023.Day13 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

parser :: Parser Text [Grid]
parser = lineGroupsP &** (pureP Text.unlines &* dotGridP)

listToMaybeE []  = Nothing
listToMaybeE [x] = Just x
listToMaybeE _   = error "listToMaybeE: more than one element"

findReflectionV :: Grid -> Maybe Int
findReflectionV g = find isReflection [succ xmin .. xmax]
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g
    ps = Map.keys g
    isReflection x = all (containsFlipped x) ps
    containsFlipped xc (Position2 x y) =
      xf < xmin || xf > xmax || Map.member (Position2 xf y) g
      where
        xf = 2 * xc - 1 - x

findReflectionH :: Grid -> Maybe Int
findReflectionH =
  findReflectionV . Map.mapKeys (\(Position2 x y) -> Position2 y x)

reflectionValue :: Grid -> Int
reflectionValue g =
  case (v, h) of
    (Just v', Nothing) -> v'
    (Nothing, Just h') -> h' * 100
    e ->
      terror $ "Unexpected reflections: " <> tshow e <> " in \n" <> displayG g
  where
    v = findReflectionV g
    h = findReflectionH g

part1 :: [Grid] -> Int
part1 = sum . map reflectionValue

tasks = Tasks 2023 13 (CodeBlock 0) parser [Task part1 405]
