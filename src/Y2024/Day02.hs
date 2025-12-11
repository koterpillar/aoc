module Y2024.Day02
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser :: Parser Text [[Int]]
parser = linesP &** integersSpaceP

part1 = countIf $ isSafeRev False

part2 = countIf $ isSafeRev True

isSafeRev damp x = isSafe1 damp x || isSafe1 damp (reverse x)

isSafe1 False = isSafe
isSafe1 True  = any (isSafe . snd) . picks

isSafe = all (uncurry safeStep) . zipTail

safeStep x1 x2 = x1 > x2 && x1 <= x2 + 3

tasks =
  Tasks
    (AOC 2024 2)
    (CodeBlock 0)
    parser
    [task part1 2 & taskPart 1, task part2 4 & taskPart 2]
