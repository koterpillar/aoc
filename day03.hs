import           Data.List

import           Text.Parsec
import           Text.Parsec.Text

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

bitsP :: Parser BitString
bitsP = many1 (char '0' *> pure O <|> char '1' *> pure I)

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

main = do
  processEI 3 (parseLines bitsP) part1 198
