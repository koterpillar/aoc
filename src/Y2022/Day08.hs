module Y2022.Day08 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 Int

visibleTrees :: Grid -> Direction4 -> Position2 -> Int -> Set Position2
visibleTrees g d p h =
  case Map.lookup p g of
    Nothing -> mempty
    Just ph
      | ph < h -> mempty
      | ph == h -> visibleTrees g d (walk d p) ph
      | otherwise -> Set.insert p $ visibleTrees g d (walk d p) ph

part1 :: Grid -> Int
part1 g =
  length $ Set.unions [visibleTrees g d p (-1) | (d, ps) <- pds, p <- ps]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    pds =
      [ (N, [Position2 x ymax | x <- [xmin .. xmax]])
      , (S, [Position2 x ymin | x <- [xmin .. xmax]])
      , (W, [Position2 xmax y | y <- [ymin .. ymax]])
      , (E, [Position2 xmin y | y <- [ymin .. ymax]])
      ]

tasks = Tasks 2022 8 (CodeBlock 0) digitGridP [Task part1 21]
