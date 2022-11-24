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

step :: Grid -> Grid
step g =
  Map.fromList $ map (\(p, v) -> (p, go v (countOccupied p))) $ Map.toList g
  where
    countOccupied p =
      length $ filter (== Occupied) $ mapMaybe (`Map.lookup` g) $ adjacent8 p
    go Empty 0 = Occupied
    go Occupied n
      | n >= 4 = Empty
    go v _ = v

stabilizedSeats :: Grid -> Int
stabilizedSeats = countIf (== Occupied) . Map.elems . fixPoint step

tasks = Tasks 2020 11 (CodeBlock 1) parseGrid [Task stabilizedSeats 37]
