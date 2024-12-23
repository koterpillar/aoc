module Y2020.Day11
  ( tasks
  ) where

import           AOC
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

countOccupied :: Foldable f => f Seat -> Int
countOccupied = countElem Occupied

occupiedNeighbors :: Grid -> Position2 -> Int
occupiedNeighbors g p = countOccupied $ mapMaybe (`mapLookup` g) $ adjacent8 p

occupiedNeighbors2 g p = countOccupied $ mapMaybe (viewRay p) allDir8
  where
    viewRay :: Position2 -> Direction8 -> Maybe Seat
    viewRay p d =
      let p' = walk d p
       in case mapLookup p' g of
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
stabilizedSeats = countOccupied . iterateSettleL (step occupiedNeighbors 4)

stabilizedSeats2 = countOccupied . iterateSettleL (step occupiedNeighbors2 5)

tasks =
  Tasks
    2020
    11
    (CodeBlock 1)
    charGridP
    [Task stabilizedSeats 37, Task stabilizedSeats2 26]
