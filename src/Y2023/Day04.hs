module Y2023.Day04 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Card =
  Card
    { cNo   :: Int
    , cWin  :: [Int]
    , cYour :: [Int]
    }
  deriving (Ord, Eq, Show)

parser :: Parser Text [Card]
parser =
  linesP &** tsplitP ": " &*
  (uncurryAgain Card <$>
   ((pureP (Text.drop 5) &* integerP) &+
    (tsplitP " | " &* (numbersP &+ numbersP))))

numbersP = tsplitSpacesP &** integerP

countWinning (Card _ w y) =
  Set.size $ Set.intersection (Set.fromList w) (Set.fromList y)

worth 0 = 0
worth w = 2 ^ (w - 1)

part1 = sum . map (worth . countWinning)

part2 cards = sum $ Map.elems $ foldl' scratch state0 cards
  where
    state0 = Map.fromList [(n, 1) | Card n _ _ <- cards]
    scratch st c@(Card n _ _) =
      Map.unionWith (+) st $
      Map.fromList [(n + i, v) | i <- [1 .. countWinning c]]
      where
        Just v = Map.lookup n st

tasks = Tasks 2023 4 (CodeBlock 0) parser [Task part1 13, Task part2 30]
