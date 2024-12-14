module Y2024.Day13
  ( tasks
  ) where

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

underLimit :: Maybe Int -> Presses -> Bool
underLimit Nothing _           = True
underLimit (Just limit) (a, b) = a <= limit && b <= limit

nextMoves :: Machine -> [Int] -> Maybe Int -> Presses -> [Presses]
nextMoves m vs limit p@(a, b) =
  filter (winnable m . target m)
    $ filter (underLimit limit)
    $ [(a + v, b) | v <- vs] ++ [(a, b + v) | v <- vs]

pressesCost :: Presses -> Int
pressesCost (a, b) = a * 3 + b

moveCost :: Presses -> Presses -> Int
moveCost = (-) `on` pressesCost

target :: Machine -> Presses -> Position2
target Machine {..} (a, b) = pointM a mA `pointPlus` pointM b mB

winning :: Maybe Int -> Machine -> Maybe Int
winning limit m = traceShowF (m, r, ) . pressesCost <$> r
  where
    steps = take 12 $ iterate (* 10) 1
    r =
      last
        <$> aStar
              (nextMoves m steps limit)
              moveCost
              distanceToGoal
              isGoal
              (0, 0)
    isGoal p = target m p == mPrize m
    distanceToGoal p =
      traceShowF ("distance estimate", p, t, prize, )
        $ manhattanDistance t prize
      where
        t = target m p
        prize = mPrize m

estimateToGoal :: Machine -> Presses -> Int
estimateToGoal m p =
  traceShowF ("distance estimate", m, p, )
    $ minimum
    $ do
        (mButton, mkPresses) <- [(mA, (, 0)), (mB, (0, ))]
        pCoord <- [pX, pY]
        let amount =
              (pCoord prize - pCoord (target m p)) `div` pCoord (mButton m)
        pure $ pressesCost $ mkPresses amount
  where
    prize = mPrize m

winnable :: Machine -> Position2 -> Bool
winnable m t
  | pX d < 0 = False
  | pY d < 0 = False
  | overshot pX pY = False
  | overshot pY pX = False
  | otherwise = True
  where
    d = mPrize m `pointMinus` t
    overshot :: (Position2 -> Int) -> (Position2 -> Int) -> Bool
    overshot pTry pResult = all (overshotAcc pTry pResult) [mA, mB]
    overshotAcc ::
         (Position2 -> Int)
      -> (Position2 -> Int)
      -> (Machine -> Position2)
      -> Bool
    overshotAcc pTry pResult mButton = maxResult < prizeResult
      where
        prizeResult = pResult (mPrize m)
        pButton = mButton m
        maxResult = pResult t + pResult pButton * maxSteps
        maxSteps = (pTry (mPrize m) - pTry t) `div` pTry pButton

part1 :: [Machine] -> Int
part1 = sum . mapMaybe (winning $ Just 100)

harderPrize :: Machine -> Machine
harderPrize m = m {mPrize = mPrize m `pointPlus` Position2 offset offset}
  where
    offset = 10000000000000

part2 :: [Machine] -> Int
part2 = sum . mapMaybe (winning Nothing . harderPrize)

testMachine :: Machine
testMachine =
  Machine {mA = Position2 10 1, mB = Position2 1 10, mPrize = Position2 p p}
  where
    p = 11

tasks =
  Tasks
    2024
    13
    (CodeBlock 0)
    parser
    [ task part1 480 & taskPart 1
    , Assert "testMachine win" (Just 4) $ winning Nothing testMachine
    , taskBlind part2 & taskPart 2
    ]
