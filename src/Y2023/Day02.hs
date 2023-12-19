module Y2023.Day02 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Gm = Gm
  { gmId    :: Int
  , gmRolls :: [Roll]
  } deriving (Show)

gameIdP :: Parser Text Int
gameIdP = pureP (Text.drop 5) &* integerP

type Roll = Map Text Int

rollP :: Parser Text Roll
rollP =
  Map.fromList
    <$> tsplitP ", " &** (tsplitP " " &* ap2P (curry swap) integerP idP)

parser :: Parser Text [Gm]
parser = linesP &** (tsplitP ": " &* ap2P Gm gameIdP (tsplitP "; " &** rollP))

bag :: Roll
bag = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

possible :: Roll -> Roll -> Bool
possible b r = all enough $ Map.toList r
  where
    enough (k, v) = Map.findWithDefault 0 k b >= v

part1 :: [Gm] -> Int
part1 = sum . map gmId . filter (all (possible bag) . gmRolls)

power :: Roll -> Int
power x = r * g * b
  where
    r = Map.findWithDefault 0 "red" x
    g = Map.findWithDefault 0 "green" x
    b = Map.findWithDefault 0 "blue" x

necessary :: [Roll] -> Roll
necessary = foldl1 (Map.unionWith max)

part2 :: [Gm] -> Int
part2 = sum . map (power . necessary . gmRolls)

tasks :: Tasks
tasks = Tasks 2023 2 (CodeBlock 0) parser [Task part1 8, Task part2 2286]
