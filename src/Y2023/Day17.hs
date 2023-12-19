module Y2023.Day17 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

type Grid = Grid2 Int

parser :: Parser Text Grid
parser = digitGridP

data St = St
  { stP :: Position2
  , stD :: Direction4
  , stL :: Int
  } deriving (Ord, Eq, Show)

instance Hashable St where
  hashWithSalt s (St p d l) = hashWithSalt s (p, d, l)

start :: St
start = St (Position2 0 0) E 0

moveForward :: St -> St
moveForward (St p d l) = St (walk d p) d (succ l)

turn :: Direction4 -> St -> St
turn d' (St p d l) = moveForward $ St p d' 0

moves :: St -> [St]
moves st@(St p d l) =
  [moveForward st | l < 3] ++ [turn d' st | d' <- [turnLeft d, turnRight d]]

solve :: (St -> [St]) -> (St -> Bool) -> Grid -> Int
solve m goalCondition g =
  ttrace (displayG $ overlayPath g path) $ sum $ map heatLossS path
  where
    path =
      fromJustE "no path"
        $ aStar moves' (const heatLossS) distanceToGoal isGoal start
    moves' = filter (insideBounds b . stP) . m
    isGoal c = distanceToGoal c == 0 && goalCondition c
    distanceToGoal (St p1 _ _) = manhattanDistance p1 goal
    b@(_, goal) = boundsG g
    heatLossS (St p _ _) = fromJustE "out of bounds" $ Map.lookup p g

overlayPath :: Grid -> [St] -> Grid2 Char
overlayPath g = foldr (Map.adjust (const 'X') . stP) (Map.map showInGrid g)

part1 :: Grid -> Int
part1 = solve moves (const True)

ultraMoves :: St -> [St]
ultraMoves st@(St p d l) =
  [moveForward st | l < 10]
    ++ [turn d' st | l >= 4, d' <- [turnLeft d, turnRight d]]

part2 :: Grid -> Int
part2 = solve ultraMoves $ \st -> stL st >= 4

tasks =
  Tasks
    2023
    17
    (CodeBlock 0)
    parser
    [task part1 102 & taskPart 1, task part2 94 & taskPart 2]
