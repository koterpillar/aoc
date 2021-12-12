module Y2021.Day06 where

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

type Pond = Map Int Int

pondP :: Parser Pond
pondP = mapByIndex <$> sepBy1 integerP (char ',')

pondStep :: Pond -> Pond
pondStep = Map.fromListWith (+) . join . map step' . Map.toList
  where
    step' (timer, count) = [(nt, count) | nt <- step timer]
    step 0 = [6, 8]
    step n = [n - 1]

pondTotal :: Pond -> Int
pondTotal = sum . Map.elems

countAfter :: Int -> Pond -> Int
countAfter days = pondTotal . iterateN days pondStep

part1 = countAfter 80

part2 = countAfter 256

tasks = Tasks 2021 6 (justParse pondP) [Task part1 5934, Task part2 26984457539]
