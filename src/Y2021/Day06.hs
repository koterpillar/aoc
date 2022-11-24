module Y2021.Day06 where

import           AOC
import           Utils

type Pond = Map Int Int

pondP :: Parser Text Pond
pondP = mapFromListCount <$> integersP ","

pondStep :: Pond -> Pond
pondStep = mapFromListSum . concatMap step' . mapToList
  where
    step' (timer, count) = [(nt, count) | nt <- step timer]
    step 0 = [6, 8]
    step n = [n - 1]

pondTotal :: Pond -> Int
pondTotal = sum

countAfter :: Int -> Pond -> Int
countAfter days = pondTotal . iterateN days pondStep

part1 = countAfter 80

part2 = countAfter 256

tasks =
  Tasks 2021 6 (CodeBlock 0) pondP [Task part1 5934, Task part2 26984457539]
