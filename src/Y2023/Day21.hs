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

type Pt = (Position2, Position2) -- (position inside grid, position in the infinite grid of grids)

type St = Map Position2 (Set Position2) -- key = inside grid, values = positions in the infinite grid of grids

type Wlk = (Direction4 -> Position2 -> Maybe Pt)

step :: Wlk -> Grid2 () -> St -> St
step wlk g st =
  Map.fromListWith Set.union $ do
    (p, gps) <- Map.toList st
    d <- allDir4
    (p', dgp) <- toList $ wlk d p
    guard $ Map.notMember p' g
    let gps' = Set.map (pointPlus dgp) gps
    pure (p', gps')

reachableInSteps :: ((Position2, Position2) -> Wlk) -> Int -> Input -> Int
reachableInSteps genWlk n i = sum $ fmap Set.size result
  where
    ib = inputBounds i
    (g, gnome) = findGnome i
    start = Map.singleton gnome (Set.singleton $ Position2 0 0)
    wlk = genWlk ib
    result = iterateNL n (step wlk g) start

wlk1 :: (Position2, Position2) -> Wlk
wlk1 ib d p
  | insideBounds ib p' = Just (p', Position2 0 0)
  | otherwise = Nothing
  where
    p' = walk d p

part1 :: Int -> Input -> Int
part1 = reachableInSteps wlk1

wlk2 :: (Position2, Position2) -> Wlk
wlk2 ib d p = Just (Position2 x'' y'', Position2 dx dy)
  where
    (Position2 0 0, Position2 xmax ymax) = ib
    Position2 x' y' = walk d p
    (dx, x'') = divMod x' (succ xmax)
    (dy, y'') = divMod y' (succ ymax)

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
