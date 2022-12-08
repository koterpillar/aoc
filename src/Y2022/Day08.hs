module Y2022.Day08 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 Int

visibleDir :: Grid -> Direction4 -> Position2 -> Int -> Grid
visibleDir g d p h =
  case Map.lookup p g of
    Nothing -> mempty
    Just ph
      | ph <= h -> rest
      | otherwise -> Map.insert p h rest
      where rest = visibleDir g d (walk d p) (max h ph)

visibleInGrid :: Grid -> Grid
visibleInGrid g =
  foldl' Map.union mempty $ [visibleDir g d p (-1) | (d, ps) <- pds, p <- ps]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    pds =
      [ (N, [Position2 x ymax | x <- [xmin .. xmax]])
      , (S, [Position2 x ymin | x <- [xmin .. xmax]])
      , (W, [Position2 xmax y | y <- [ymin .. ymax]])
      , (E, [Position2 xmin y | y <- [ymin .. ymax]])
      ]

type VGrid = Grid2 (Int, Bool)

withVisible :: Grid -> Grid -> VGrid
withVisible g vg = Map.mapWithKey go g
  where
    go k v = (v, Map.member k vg)

instance GridItem (Int, Bool) where
  showInGrid (i, True)  = head $ show i
  showInGrid (i, False) = "₀₁₂₃₄₅₆₇₈₉" !! i

traceWithVisible :: Grid -> Grid
traceWithVisible g = ttrace (displayG vv) vg
  where
    vg = visibleInGrid g
    vv = withVisible g vg

part1 :: Grid -> Int
part1 = length . Map.keysSet . traceWithVisible

scenicScore :: Grid -> Position2 -> Int
scenicScore g p = product $ [go g d (walk d p) | d <- [N, S, E, W]]
  where
    h0 = fromJustE "h0" $ Map.lookup p g
    go g d p =
      case Map.lookup p g of
        Nothing -> 0
        Just h
          | h >= h0 -> 1
          | otherwise -> 1 + go g d (walk d p)

part2 :: Grid -> Int
part2 g = maximum $ map (scenicScore g) $ Map.keys g

tasks = Tasks 2022 8 (CodeBlock 0) digitGridP [Task part1 21, Task part2 8]
