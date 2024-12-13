module Y2024.Day13 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

data Machine = Machine
  { mA     :: Position2
  , mB     :: Position2
  , mPrize :: Position2
  } deriving (Ord, Eq, Show)

machineLineP :: Parser Text Position2
machineLineP = pureP cleanup &* wordsP &* ap2P Position2 integerP integerP
  where
    cleanup =
      Text.map
        (\c ->
           if not $ isDigit c
             then ' '
             else c)

parser :: Parser Text [Machine]
parser = lineGroupsP &** ap3P Machine machineLineP machineLineP machineLineP

type Presses = (Int, Int) -- number of A and B

maxPresses :: Int
maxPresses = 100

lessThan :: Maybe Int -> Int -> Bool
lessThan limit n = all (n <) limit

nextMoves :: Maybe Int -> Presses -> [Presses]
nextMoves limit (a, b) =
  [(succ a, b) | lessThan limit a] ++ [(a, succ b) | lessThan limit b]

pressesCost :: Presses -> Int
pressesCost (a, b) = a * 3 + b

moveCost :: Presses -> Presses -> Int
moveCost = (-) `on` pressesCost

target :: Machine -> Presses -> Position2
target Machine {..} (a, b) = pointM a mA `pointPlus` pointM b mB

winning :: Maybe Int -> Machine -> Maybe Int
winning limit m@Machine {..} = traceShowF (m, r, ) . pressesCost <$> r
  where
    Position2 px py = mPrize
    r = last <$> aStar (nextMoves limit) moveCost distanceToGoal isGoal (0, 0)
    isGoal p = target m p == mPrize
    distanceToGoal p
      | tx <= px && ty <= py = px - tx + py - ty
      | otherwise = (px + py) * 10
      where
        Position2 tx ty = target m p

part1 :: [Machine] -> Int
part1 = sum . mapMaybe (winning $ Just 100)

tasks = Tasks 2024 13 (CodeBlock 0) parser [task part1 480 & taskPart 1]
