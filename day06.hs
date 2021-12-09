import           Control.Monad    (join)

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

type Pond = Map Int Int

mkPond :: [Int] -> Pond
mkPond = foldr (\timer -> Map.insertWith (+) timer 1) Map.empty

pondP :: Parser Pond
pondP = mkPond <$> sepBy1 integerP (char ',')

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

main = do
  processEI 6 (justParse pondP) part1 5934
  processEI 6 (justParse pondP) part2 26984457539
