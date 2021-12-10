import           Control.Monad    (join)

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

import           Debug.Trace

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

fuelToZero :: Crabs -> Int
fuelToZero = go 0
  where
    go :: Int -> Crabs -> Int
    go pos []     = 0
    go pos (c:cs) = pos * c + go (pos + 1) cs

part1 :: Crabs -> Int
part1 crabs = go (fuelToZero crabs) 0 totalcrabs crabs
  where
    totalcrabs = sum crabs
    go :: Int -> Int -> Int -> Crabs -> Int
    go tohere _ 0 [] = tohere
    go _ _ cttr [] =
      error $ "unexpected " <> show cttr <> " crabs to the right of the end."
    go tohere cttl cttr (c:cs) =
      if tohere' > tohere
        then tohere
        else go tohere' cttl' cttr' cs
      where
        tohere' = tohere + cttl' - cttr'
        cttl' = cttl + c
        cttr' = cttr - c

main = do
  assertEqual "groupPositions" [2, 0, 1] $ groupPositions [0, 2, 0]
  assertEqual "Fuel to zero" 13 $ fuelToZero [0, 0, 2, 3]
  processEI 7 (justParse crabsP) part1 37
