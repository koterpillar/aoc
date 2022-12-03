module Y2022.Day03 where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type Item = Char

type Rucksack = (Set Item, Set Item)

priority :: Item -> Int
priority c
  | inRange 'a' 'z' c = ord c - ord 'a' + 1
  | inRange 'A' 'Z' c = ord c - ord 'A' + 27
  | otherwise = error "invalid item"

parser :: Parser Text [Rucksack]
parser =
  linesP &** charactersP &* pureP splitHalf &* pureP Set.fromList &=
  pureP Set.fromList
  where
    splitHalf t = splitAt l t
      where
        l = length t `div` 2

part1 :: [Rucksack] -> Int
part1 = sum . map (sum . Set.map priority . uncurry Set.intersection)

part2 :: [Rucksack] -> Int
part2 =
  sum .
  map
    (sum . Set.map priority . foldl1 Set.intersection . map (uncurry Set.union)) .
  chunksOf 3

tasks = Tasks 2022 3 (CodeBlock 0) parser [Task part1 157, Task part2 70]
