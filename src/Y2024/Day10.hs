module Y2024.Day10
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

parser :: Parser Text (Grid2 Int)
parser = digitGridP

trailHeads :: Grid2 Int -> [Position2]
trailHeads = filterTuple (== 0) . Map.toList

type TrailEval a = Set Position2 -> Position2 -> a

tEnd :: TrailEval Position2
tEnd _ end = end

tPath :: TrailEval (Set Position2)
tPath path end = Set.insert end path

trailScore ::
     forall a. (Ord a, Show a)
  => TrailEval a
  -> Grid2 Int
  -> Position2
  -> Int
trailScore te g = Set.size . go Set.empty
  where
    go :: Set Position2 -> Position2 -> Set a
    go visited pos =
      case Map.lookup pos g of
        Just 9 -> Set.singleton $ te visited pos
        Nothing -> Set.empty
        Just v ->
          Set.unions $ do
            let visited' = Set.insert pos visited
            dir <- allDir4
            let pos' = walk dir pos
            guard $ Map.lookup pos' g == Just (succ v)
            pure $ go visited' pos'

part :: (Grid2 Int -> Position2 -> Int) -> Grid2 Int -> Int
part trailEval g = sum $ traceShowId $ map (trailEval g) $ trailHeads g

part1 :: Grid2 Int -> Int
part1 = part $ trailScore tEnd

part2 :: Grid2 Int -> Int
part2 = part $ trailScore tPath

tasks =
  Tasks
    2024
    10
    (CodeBlock 4)
    parser
    [ task part1 36 & taskPart 1
    , task part2 3 & taskScraper (CodeBlock 5)
    , task part2 13 & taskScraper (CodeBlock 7)
    , task part2 227 & taskScraper (CodeBlock 8)
    , task part2 81 & taskPart 2
    ]
