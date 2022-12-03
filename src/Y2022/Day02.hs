module Y2022.Day02 where

import           AOC
import           Utils

data RPS
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq, Ord, Enum, Bounded)

rpsP :: Parser Text RPS
rpsP =
  charP &* choiceP (zip "ABCXYZ" [Rock, Paper, Scissors, Rock, Paper, Scissors])

score :: RPS -> RPS -> Int
score x y = winScore x y + shapeScore y

winScore Rock Paper        = 6
winScore Paper Scissors    = 6
winScore Scissors Rock     = 6
winScore Rock Rock         = 3
winScore Paper Paper       = 3
winScore Scissors Scissors = 3
winScore _ _               = 0

shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

findWS x s = headE "no score found" [y | y <- boundedAll, winScore x y == s]

modifyShape :: RPS -> RPS -> (RPS, RPS)
modifyShape x Rock     = (x, findWS x 0)
modifyShape x Paper    = (x, findWS x 3)
modifyShape x Scissors = (x, findWS x 6)

parser :: Parser Text [(RPS, RPS)]
parser = linesP &** wordsP &* rpsP &+ rpsP

part1 = sum . map (uncurry score)

part2 = sum . map (uncurry score . uncurry modifyShape)

tasks = Tasks 2022 2 (CodeBlock 0) parser [Task part1 15, Task part2 12]
