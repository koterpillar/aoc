module Y2024.Day06 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Itm
  = Obstacle
  | PossibleObstacle
  | Guard Direction4
  deriving (Eq, Ord, Show)

instance Bounded Itm where
  minBound = Obstacle
  maxBound = Guard maxBound

instance Enum Itm where
  toEnum 0 = Obstacle
  toEnum 1 = PossibleObstacle
  toEnum n = Guard (toEnum (n - 2))
  fromEnum Obstacle         = 0
  fromEnum PossibleObstacle = 1
  fromEnum (Guard d)        = fromEnum d + 2

instance GridItem Itm where
  showInGrid Obstacle         = '#'
  showInGrid PossibleObstacle = 'O'
  showInGrid (Guard d)        = showInGrid d

type Input = Grid2 Itm

type GuardPos = (Position2, Direction4)

type Input2 = (GuardPos, Grid2 ())

convertInput :: Input -> Input2
convertInput g0 = (pd, g)
  where
    g = void $ Map.filter (== Obstacle) g0
    pd =
      fromJustE "No guard"
        $ listToMaybe [(p, d) | (p, Guard d) <- Map.toList g0]

parser :: Parser Text Input2
parser = convertInput <$> charGridP

part1 :: Input2 -> Int
part1 (start, g) = Set.size $ go start
  where
    bounds = boundsG g
    go (p, d)
      | not $ insideBounds bounds p = Set.empty
      | Map.member pS g = Set.insert p $ go (p, dT)
      | otherwise = Set.insert p $ go (pS, d)
      where
        pS = walk d p
        dT = turnRight d

showPossible :: Grid2 () -> [Position2] -> Grid2 Itm
showPossible g = foldr (`Map.insert` PossibleObstacle) (fmap (const Obstacle) g)

part2 :: Input2 -> Int
part2 (start, g) =
  length $ ttraceF (displayG . showPossible g) $ go start Set.empty []
  where
    bounds = boundsG g
    go :: GuardPos -> Set GuardPos -> [Position2] -> [Position2]
    go (p, d) prev res
      | not $ insideBounds bounds p = res -- went out
      | Map.member pS g = go (p, dT) prev' res -- existing obstacle
      | Set.member (p, dT) prev = go (pS, d) prev' (pS : res) -- if we turned here it would be a loop
      | otherwise = go (pS, d) prev' res
      where
        pS = walk d p
        dT = turnRight d
        prev' = Set.insert (p, d) prev

tasks =
  Tasks
    2024
    6
    (CodeBlock 0)
    parser
    [task part1 41 & taskPart 1, task part2 6 & taskPart 2]
