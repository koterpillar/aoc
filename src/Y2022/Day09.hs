module Y2022.Day09 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Move = (Direction4, Int)

parser :: Parser Text [Move]
parser = linesP &** wordsP &* ((charP &* choiceEBP "RULD") &+ integerP)

type RopeState = [Position2]

initRope :: Int -> RopeState
initRope n = replicate n $ Position2 0 0

foldCollect :: (b -> a -> a) -> a -> [b] -> [a]
foldCollect _ a []     = [a]
foldCollect f a (b:bs) = a : foldCollect f (f b a) bs

dragTowards :: Int -> Int -> Int
dragTowards a b
  | b > a = pred b
  | b < a = succ b
  | otherwise = b

dragTowardsP :: Position2 -> Position2 -> Position2
dragTowardsP p1@(Position2 x1 y1) p2@(Position2 x2 y2)
  | hammingDistance p1 p2 <= 1 = p2
  | otherwise = Position2 (dragTowards x1 x2) (dragTowards y1 y2)

dragRope :: Direction4 -> RopeState -> RopeState
dragRope d (h:t) =
  let r' = walk d h : zipWith dragTowardsP r' t
   in r'

mkSteps :: [Move] -> [Direction4]
mkSteps = concatMap $ \(d, n) -> replicate n d

traceTail :: Int -> [Move] -> Int
traceTail n =
  length .
  ttraceF (displayG . fromSetG) .
  Set.fromList . map last . foldCollect dragRope (initRope n) . mkSteps

part1 :: [Move] -> Int
part1 = traceTail 2

part2 :: [Move] -> Int
part2 = traceTail 10

tasks =
  Tasks
    2022
    9
    (CodeBlock 3)
    parser
    [ Assert
        "drag rope 1"
        [Position2 2 0, Position2 1 0]
        (dragRope E [Position2 1 0, Position2 0 0])
    , Assert
        "drag rope 2"
        [Position2 2 1, Position2 1 1]
        (dragRope E [Position2 1 1, Position2 0 0])
    , Task part1 13
    , TaskScraper (CodeBlock 7) part2 36
    ]
