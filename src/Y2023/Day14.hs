module Y2023.Day14 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Cycle
import           Grid
import           Utils

data Rock
  = O
  | X
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Rock where
  showInGrid O = 'O'
  showInGrid X = '#'

type Grid1 = Grid2 Rock

data RockGrid =
  RockGrid
    { rgBounds :: (Position2, Position2)
    , rgX      :: Grid2 ()
    , rgO      :: Grid2 ()
    }
  deriving (Eq, Ord, Show)

toRockGrid :: Grid1 -> RockGrid
toRockGrid g = RockGrid {..}
  where
    rgX = Map.fromList [(p, ()) | (p, X) <- Map.toList g]
    rgO = Map.fromList [(p, ()) | (p, O) <- Map.toList g]
    rgBounds = boundsG g

parser :: Parser Text RockGrid
parser = charGridP &* pureP toRockGrid

roll :: Direction4 -> RockGrid -> RockGrid
roll d (RockGrid b gx go) = RockGrid b gx go'
  where
    stops = mapFromListCount $ map findStop $ Map.keys go
    go' =
      Map.fromList
        [(walkN i d' p, ()) | (p, n) <- Map.toList stops, i <- [1 .. n]]
    d' = reverse4 d
    findStop p =
      fromJustE "findStop" $
      find (\p' -> not (insideBounds b p') || Map.member p' gx) $
      iterate (walk d) p

northLoad :: RockGrid -> Int
northLoad (RockGrid (_, Position2 _ ymax) _ go) =
  sum [ymax - y + 1 | Position2 _ y <- Map.keys go]

part1 = northLoad . roll N

rollCycle = iterateNL 1 $ roll E . roll S . roll W . roll N

part2 =
  northLoad .
  cycleElement 1000000000 .
  traceShowF (\c -> (length (cycleStart c), length (cycleLoop c))) .
  cycleFind rollCycle

tasks = Tasks 2023 14 (CodeBlock 0) parser [Task part1 136, Task part2 64]
