module Y2021.Day03 where

import           AOC
import           Bit
import           Utils

leastCommon :: [Bit] -> Maybe Bit
leastCommon = fmap bitInvert . mostCommon

part1 :: [BitString] -> Int
part1 input =
  let bybit = transpose input
      gamma = bitsValue $ map (fromJust . mostCommon) bybit
      epsilon = bitsValue $ map (fromJust . leastCommon) bybit
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
    oxygen = bitsValue $ byCriteria (fromJust . mostCommon) input
    co2 = bitsValue $ byCriteria (fromJust . leastCommon) input

tasks =
  Tasks 2021 3 LastCodeBlock (linesP &** bitsP) [task part1 198, task part2 230]
