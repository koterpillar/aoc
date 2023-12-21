module Y2023.Day21 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data GardenItem
  = Sand
  | Gnome
  deriving (Ord, Eq, Show, Enum, Bounded)

instance GridItem GardenItem where
  showInGrid Sand  = '#'
  showInGrid Gnome = 'S'

type Input = Grid2 GardenItem

parser :: Parser Text Input
parser = charGridP

inputBounds :: Input -> (Position2, Position2)
inputBounds input =
  (Position2 (pred xmin) (pred ymin), Position2 (succ xmax) (succ ymax))
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG input

findGnome :: Input -> (Grid2 (), Position2)
findGnome i = (g, start)
  where
    (start, Gnome) = fromSingleE "gnome" $ Map.toList $ Map.filter (== Gnome) i
    g = void $ Map.filter (/= Gnome) i

step :: Grid2 () -> Set Position2 -> Set Position2
step g =
  Set.fromList
    . filter (`Map.notMember` g)
    . concatMap (\p -> [walk d p | d <- allDir4])
    . Set.toList

part1 :: Int -> Input -> Int
part1 n i = length result
  where
    (g, start) = second Set.singleton $ findGnome i
    result = iterateNL n (step g) start

tasks =
  Tasks
    2023
    21
    (CodeBlock 0)
    parser
    [AssertExample "part 1" 16 (part1 6), taskBlind (part1 64) & taskPart 1]
