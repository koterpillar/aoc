{-# LANGUAGE ScopedTypeVariables #-}

module Y2023.Day21 where

import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text

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

inputMaxBound :: Input -> Position2
inputMaxBound input = Position2 (succ xmax) (succ ymax)
  where
    (Position2 1 1, Position2 xmax ymax) = boundsG input

type Grid = Grid2 ()

findGnome :: Input -> (Grid, Position2)
findGnome i = (g, start)
  where
    (start, Gnome) = fromSingleE "gnome" $ Map.toList $ Map.filter (== Gnome) i
    g = void $ Map.filter (/= Gnome) i

type It = (Position2, Set Position2) -- (position inside grid, positions in the infinite grid of grids)

type St = Map Position2 (Set Position2) -- key = inside grid, values = positions in the infinite grid of grids

type Wlk = (Direction4 -> Position2 -> Maybe (Position2, Position2))

step :: Wlk -> St -> St
step wlk st =
  Map.fromListWith Set.union $ do
    (p, gps) <- Map.toList st
    d <- allDir4
    (p', dgp) <- toList $ wlk d p
    let gps' = Set.map (pointPlus dgp) gps
    pure (p', gps')

reachableInSteps :: (Grid -> Position2 -> Wlk) -> Int -> Input -> Int
reachableInSteps genWlk n i = sum $ fmap Set.size result
  where
    im = inputMaxBound i
    (g, gnome) = findGnome i
    start = Map.singleton gnome (Set.singleton $ Position2 0 0)
    wlk = genWlk g im
    result = iterateNL n (step wlk) start

wlk1 :: Grid -> Position2 -> Wlk
wlk1 g im d p =
  wlk2 g im d p >>= \case
    r@(p', Position2 0 0) -> Just r
    _ -> Nothing

wlk2 :: Grid -> Position2 -> Wlk
wlk2 g im d p = do
  let Position2 xmax ymax = im
  let Position2 x' y' = walk d p
  let (dx, x'') = divMod x' (succ xmax)
  let (dy, y'') = divMod y' (succ ymax)
  let p'' = Position2 x'' y''
  guard $ Map.notMember p'' g
  let dp = Position2 dx dy
  pure (p'', dp)

part1 :: Int -> Input -> Int
part1 = reachableInSteps wlk1

part2 :: Int -> Input -> Int
part2 = reachableInSteps wlk2

tasks =
  Tasks
    2023
    21
    (CodeBlock 0)
    parser
    [ AssertExample "part 1" 16 $ part1 6
    , taskBlind (part1 64) & taskPart 1
    , AssertExample "part 2 1" 2 $ part2 1
    , AssertExample "part 2 6" 16 $ part2 6
    , AssertExample "part 2 10" 50 $ part2 10
    , AssertExample "part 2 100" 6536 $ part2 100
    , AssertExample "part 2 500" 167004 $ part2 500
    -- , AssertExample "part 2 1000" 668697 $ part2 1000
    -- , taskBlind (part2 26501365) & taskPart 2
    ]
