module Y2021.Day03 where

import           AOC
import           Utils

data Bit
  = O
  | I
  deriving (Ord, Eq, Show)

bitInvert O = I
bitInvert I = O

type BitString = [Bit]

bitsValue :: BitString -> Int
bitsValue = foldl bitMult 0
  where
    bitMult n O = n * 2
    bitMult n I = n * 2 + 1

bitsP :: Parser Text BitString
bitsP = charactersP &** choiceP [("0", O), ("1", I)]

mostCommon :: [Bit] -> Bit
mostCommon = choose . foldr count (0, 0)
  where
    count O (c0, c1) = (c0 + 1, c1)
    count I (c0, c1) = (c0, c1 + 1)
    choose (c0, c1)
      | c0 > c1 = O
      | otherwise = I

leastCommon :: [Bit] -> Bit
leastCommon = bitInvert . mostCommon

part1 :: [BitString] -> Int
part1 input =
  let bybit = transpose input
      gamma = bitsValue $ map mostCommon bybit
      epsilon = bitsValue $ map leastCommon bybit
   in gamma * epsilon

byCriteria :: ([Bit] -> Bit) -> [BitString] -> BitString
byCriteria = byCriteria' 0

byCriteria' :: Int -> ([Bit] -> Bit) -> [BitString] -> BitString
byCriteria' _ _ [] = error "Selected zero candidates."
byCriteria' _ _ [candidate] = candidate
byCriteria' bitIndex chooseBit candidates
  | bitIndex >= length (head candidates) =
    error "More than one candidate remaining, no bits left."
  | otherwise = byCriteria' (bitIndex + 1) chooseBit remainingCandidates
  where
    selectBit bs = bs !! bitIndex
    bitsSelected = map selectBit candidates
    resultBit = chooseBit bitsSelected
    remainingCandidates = filter (\bs -> selectBit bs == resultBit) candidates

part2 input = oxygen * co2
  where
    oxygen = bitsValue $ byCriteria mostCommon input
    co2 = bitsValue $ byCriteria leastCommon input

tasks = Tasks 2021 3 (linesP &** bitsP) [Task part1 198, Task part2 230]
