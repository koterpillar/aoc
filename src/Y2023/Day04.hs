module Y2023.Day04 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Card =
  Card
    { cWin  :: [Int]
    , cYour :: [Int]
    }
  deriving (Ord, Eq, Show)

parser :: Parser Text [Card]
parser =
  linesP &**
  (tsplitP ": " &* pureP (head . tail) &*
   (tsplitP " | " &* ap2P Card numsP numsP))

numsP = tsplitP " " &* pureP (filter $ not . Text.null) &** integerP

countWinning (Card w y) =
  Set.size $ Set.intersection (Set.fromList w) (Set.fromList y)

worth 0 = 0
worth w = 2 ^ (w - 1)

part1 = sum . map (worth . countWinning)

tasks = Tasks 2023 4 (CodeBlock 0) parser [Task part1 13]
