module Y2024.Day23
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Graph
import           Memo
import           Utils

type Input = Graph Text

parser :: Parser Text Input
parser = fmap mkGraph $ linesP &** (tsplitP "-" &* (idP &+ idP))
  where
    mkGraph = bidirectional . mapFromListS . map (second Set.singleton)

hasCH :: Set Text -> Bool
hasCH c = any (Text.isPrefixOf "t") (Set.toList c)

triad :: Ord a => a -> Graph a -> [Set a]
triad a g = do
  let v = Set.toList $ neighbors a g
  (b, v') <- picks v
  c <- v'
  guard $ isNeighbor b c g
  pure $ Set.fromList [a, b, c]

triads :: Ord a => Graph a -> [Set a]
triads g = nubOrd $ concatMap (`triad` g) $ Map.keys g

part1 :: Input -> Int
part1 = countIf hasCH . triads

part2 :: Input -> Text
part2 =
  Text.intercalate "," . toList . maximumOn length . stronglyConnectedComponents

tasks =
  Tasks
    2024
    23
    (CodeBlock 0)
    parser
    [task part1 7 & taskPart 1, task part2 "co,de,ka,ta" & taskPart 2]
