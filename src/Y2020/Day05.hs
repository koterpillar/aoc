module Y2020.Day05 where

import qualified Data.Text as Text

import           Numeric

import           AOC
import           Utils

parseCodes :: Parser Text [Int]
parseCodes = linesP &** pureP seatCode

seatCode :: Text -> Int
seatCode =
  treadBin .
  treplace "F" "0" . treplace "B" "1" . treplace "L" "0" . treplace "R" "1"

treadBin :: Text -> Int
treadBin = fst . head . readBin . Text.unpack

missing :: [Int] -> Int
missing = go . sort
  where
    go (x1:x2:xs)
      | x2 == x1 + 1 = go (x2 : xs)
      | otherwise = x1 + 1
    go _ = error "can't find missing seat"

tasks =
  Tasks
    2020
    5
    (Inline "FBFBBFFRLR\nFBFBBFFLRR")
    parseCodes
    [Task maximum 357, Task missing 356]
