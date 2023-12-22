{-# LANGUAGE ScopedTypeVariables #-}

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
  | Reach
  deriving (Ord, Eq, Show, Enum, Bounded)

instance GridItem GardenItem where
  showInGrid Sand  = '#'
  showInGrid Gnome = 'S'
  showInGrid Reach = 'O'

type Input = Grid2 GardenItem

parser :: Parser Text Input
parser = charGridP

inputMaxBound :: Input -> Position2
inputMaxBound input = Position2 (succ xmax) (succ ymax)
  where
    (Position2 1 1, Position2 xmax ymax) = boundsG input

type Grid = Grid2 ()

findGnomeCenter :: Input -> Grid
findGnomeCenter i = g
  where
    (start, Gnome) = fromSingleE "gnome" $ Map.toList $ Map.filter (== Gnome) i
    g = Map.mapKeys (`pointMinus` start) $ void $ Map.filter (/= Gnome) i

stepPoint :: Grid -> Position2 -> [Position2]
stepPoint g p = do
  d <- allDir4
  let p' = walk d p
  guard $ Map.notMember p' g
  pure p'

step1 :: Grid -> Set Position2 -> Set Position2
step1 g st =
  Set.fromList $ do
    p <- Set.toList st
    stepPoint g p

stepN :: Grid -> Int -> Set Position2 -> Set Position2
stepN _ 0 ps = ps
stepN g n ps = stepN g (pred n) $ step1 g ps

nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge a b = nubOrd $ a ++ b

mkGarden :: Grid -> Set Position2 -> Input
mkGarden g st = Map.map (const Sand) g `Map.union` Map.fromSet (const Reach) st

reachableInSteps :: Int -> Input -> Set Position2
reachableInSteps n i =
  ttraceF (displayG . mkGarden g) $ stepN g n (Set.singleton $ Position2 0 0)
  where
    im = inputMaxBound i
    g = findGnomeCenter i

part1 :: Int -> Input -> Int
part1 n = length . reachableInSteps n

part2 = error "error"

tasks =
  Tasks
    2023
    21
    (CodeBlock 0)
    parser
    [ AssertExample "part 1" 16 $ part1 6
    , taskBlind (part1 64) & taskPart 1
    , AssertExample "part 2 1" 2 $ part2 1
    , AssertExample "part 2 2" 4 $ part2 2
    , AssertExample "part 2 6" 16 $ part2 6
    , AssertExample "part 2 10" 50 $ part2 10
    , AssertExample "part 2 100" 6536 $ part2 100
    , AssertExample "part 2 500" 167004 $ part2 500
    -- , AssertExample "part 2 1000" 668697 $ part2 1000
    -- , taskBlind (part2 26501365) & taskPart 2
    ]
