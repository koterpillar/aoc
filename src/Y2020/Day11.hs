module Y2020.Day11 where

import qualified Data.Map as Map

import           AOC
import           Graph
import           Grid
import           Utils

data Seat
  = Floor
  | Empty
  | Occupied
  deriving (Eq, Show, Bounded, Enum)

instance GridItem Seat where
  showInGrid Floor    = '.'
  showInGrid Empty    = 'L'
  showInGrid Occupied = '#'

type Grid = Grid2 Seat

parseGrid :: Parser Text Grid
parseGrid = fromMatrixG <$> linesP &** (charactersP &** choiceEBP ".L#")

countOccupied :: [Seat] -> Int
countOccupied = countIf (== Occupied)

occupiedNeighbors :: Grid -> Position2 -> Int
occupiedNeighbors g p = countOccupied $ mapMaybe (`Map.lookup` g) $ adjacent8 p

occupiedNeighbors2 g p = countOccupied $ mapMaybe (viewRay p) allDir8
  where
    viewRay :: Position2 -> Direction8 -> Maybe Seat
    viewRay p d =
      let p' = walk d p
       in case Map.lookup p' g of
            Just Floor -> viewRay p' d
            x          -> x

step :: (Grid -> Position2 -> Int) -> Int -> Grid -> Grid
step occupiedNeighbors maxOccupied g =
  mapFromList $ map (\(p, v) -> (p, go v (occupiedNeighbors g p))) $ mapToList g
  where
    go Empty 0 = Occupied
    go Occupied n
      | n >= maxOccupied = Empty
    go v _ = v

stabilizedSeats, stabilizedSeats2 :: Grid -> Int
stabilizedSeats =
  countOccupied . Map.elems . iterateSettle (step occupiedNeighbors 4)

stabilizedSeats2 =
  countOccupied . Map.elems . iterateSettle (step occupiedNeighbors2 5)

tasks =
  Tasks
    2020
    11
    (CodeBlock 1)
    parseGrid
    [Task stabilizedSeats 37, Task stabilizedSeats2 26]
