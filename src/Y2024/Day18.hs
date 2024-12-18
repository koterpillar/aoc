module Y2024.Day18
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

parser :: Parser Text [Position2]
parser = linesP &** (integersP "," &* ap2P Position2 idP idP)

isExample :: [Position2] -> Bool
isExample = (== 6) . maximum . map pX

findPath :: Int -> Int -> [Position2] -> Maybe [Position2]
findPath cnt sz falls = aStarDepthGoal moves estimate start
  where
    start = Position2 0 0
    corrupted = Set.fromList $ take cnt falls
    target = Position2 sz sz
    estimate = manhattanDistance target
    moves p = do
      d <- allDir4
      let p' = walk d p
      guard $ pX p' >= 0 && pY p' >= 0 && pX p' <= sz && pY p' <= sz
      guard $ Set.notMember p' corrupted
      pure p'

displayCorrupted :: Set Position2 -> Text
displayCorrupted = displayG . Map.fromSet (const ())

part1 :: [Position2] -> Int
part1 falls = length $ fromJustE "part1 result" $ findPath cnt sz falls
  where
    cnt
      | isExample falls = 12
      | otherwise = 1024
    sz
      | isExample falls = 6
      | otherwise = 70

part2 :: [Position2] -> Text
part2 falls = format $ (falls !!) $ go 0 (length falls)
  where
    format :: Position2 -> Text
    format (Position2 x y) = Text.pack $ show x <> "," <> show y
    go :: Int -> Int -> Int
    go n1 n2
      | n2 == succ n1 = n1
      | otherwise =
        let m = (n1 + n2) `div` 2
         in if isJust $ findPath m sz falls
              then go m n2
              else go n1 m
    sz
      | isExample falls = 6
      | otherwise = 70

tasks =
  Tasks
    2024
    18
    (CodeBlock 0)
    parser
    [task part1 22 & taskPart 1, task part2 "6,1" & taskPart 2]
