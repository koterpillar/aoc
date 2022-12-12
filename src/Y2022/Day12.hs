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

findP g t = fst $ fromJustE "findP" $ find (\(_, c) -> c == t) $ Map.toList g

findPath :: Grid -> Maybe [Position2]
findPath g = findPath' g (findP g 'S')

findPath' :: Grid -> Position2 -> Maybe [Position2]
findPath' g = aStarDepth moves distanceToGoal isGoal
  where
    endP = findP g 'E'
    isGoal p = Map.lookup p g == Just 'E'
    distanceToGoal = manhattanDistance endP
    moves p = do
      let h = mapLookupE "moves p" p g
      d <- allDir4
      let p' = walk d p
      h' <- maybeToList $ Map.lookup p' g
      guard $ height h' <= succ (height h)
      pure p'

part1 = length . fromJustE "part1: no path" . findPath

starts :: Grid -> [Position2]
starts g = [p | p <- Map.keys g, fmap height (Map.lookup p g) == Just 'a']

findPaths :: Grid -> [Maybe [Position2]]
findPaths g = map (findPath' g) (starts g)

part2 = minimum . map length . catMaybes . findPaths

tasks = Tasks 2022 12 (CodeBlock 0) parser [Task part1 31, Task part2 29]
