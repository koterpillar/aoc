module Y2021.Day07 where

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

type Crabs = [Int]

-- | Group items in the list by their position
groupPositions :: [Int] -> [Int]
groupPositions = go 0 . Map.toList . mapByIndex
  where
    go _ [] = []
    go n rest@((n', x):xs)
      | n == n' = x : go (n + 1) xs
      | otherwise = 0 : go (n + 1) rest

crabsP :: Parser Crabs
crabsP = groupPositions <$> sepBy1 integerP (char ',')

part1 :: Crabs -> Int
part1 = minimum . fuelOptions id

fuelOptions :: (Int -> Int) -> Crabs -> [Int]
fuelOptions consumption = go []
  where
    go :: Crabs -> Crabs -> [Int]
    go _ []          = []
    go cttl (c:cttr) = fuel2 1 cttl + fuel2 1 cttr : go (c : cttl) cttr
    fuel2 _ []            = 0
    fuel2 distance (c:cs) = c * consumption distance + fuel2 (distance + 1) cs

fuelConsumption2 :: Int -> Int
fuelConsumption2 distance = distance * (distance + 1) `div` 2

part2 :: Crabs -> Int
part2 = minimum . fuelOptions fuelConsumption2

tasks =
  Tasks
    2021
    7
    (justParse crabsP)
    [ Assert "groupPositions" [2, 0, 1] $ groupPositions [0, 2, 0]
    , Assert "Fuel consumption 2 for 1" 1 $ fuelConsumption2 1
    , Assert "Fuel consumption 2 for 4" (1 + 2 + 3 + 4) $ fuelConsumption2 4
    , Task part1 37
    , Task part2 168
    ]
