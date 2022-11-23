module Y2020.Day05 where

import qualified Data.Text as Text

import           Numeric

import           AOC
import           Utils

parseCodes :: Parser Text [Text]
parseCodes = linesP

seatCode :: Text -> Int
seatCode =
  readBin .
  Text.replace "F" "0" .
  Text.replace "B" "1" . Text.replace "L" "0" . Text.replace "R" "1"

readBin :: Text -> Int
readBin =
  fst .
  head .
  readInt 2 (`elem` ("01" :: String)) (\c -> ord c - ord '0') . Text.unpack

tasks =
  Tasks
    2020
    5
    (Inline "FBFBBFFRLR")
    parseCodes
    [Task (maximum . map seatCode) 357]
