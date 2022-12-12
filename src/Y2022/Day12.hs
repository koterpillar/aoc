module Y2022.Day12 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

type Grid = Grid2 Char

height :: Char -> Char
height 'S' = 'a'
height 'E' = 'z'
height c
  | inRange 'a' 'z' c = c
  | otherwise = error $ "height: invalid char: " ++ show c

parser :: Parser Text Grid
parser = fromMatrixG <$> linesP &** charactersP

findPath :: Grid -> Maybe [Position2]
findPath = findPath' <*> mapFindValueE "find start" (== 'S')

findPath' :: Grid -> Position2 -> Maybe [Position2]
findPath' g = aStarDepth moves distanceToGoal isGoal
  where
    endP = mapFindValueE "find end" (== 'E') g
    isGoal = (== endP)
    distanceToGoal = manhattanDistance endP
    moves p = do
      let h = mapLookupE "moves p" p g
      d <- allDir4
      let p' = walk d p
      h' <- maybeToList $ mapLookup p' g
      guard $ height h' <= succ (height h)
      pure p'

part1 = length . fromJustE "part1: no path" . findPath

starts :: Grid -> [Position2]
starts = mapFilterValues ((== 'a') . height)

findPaths :: Grid -> [Maybe [Position2]]
findPaths = map <$> findPath' <*> starts

part2 = minimum . map length . catMaybes . findPaths

tasks = Tasks 2022 12 (CodeBlock 0) parser [Task part1 31, Task part2 29]
