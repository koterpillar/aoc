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

data Grid = Grid
  { gG      :: Grid2 ()
  , gBounds :: (Position2, Position2)
  } deriving (Eq, Ord, Show)

findGnomeCenter :: Input -> Grid
findGnomeCenter i = Grid {..}
  where
    (start, Gnome) = fromSingleE "gnome" $ Map.toList $ Map.filter (== Gnome) i
    gG = Map.mapKeys (`pointMinus` start) $ void $ Map.filter (/= Gnome) i
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG gG
    gBounds =
      (Position2 (pred xmin) (pred ymin), Position2 (succ xmax) (succ ymax))

parser :: Parser Text Grid
parser = findGnomeCenter <$> charGridP

testInput :: Grid
testInput =
  justParse parser
    $ Text.unlines
        [ "####.##.#"
        , "####.#..."
        , "####.#..#"
        , "####.####"
        , "....S...."
        , "......#.."
        , ".....####"
        , "......#.."
        , "......#.."
        ]

stepPoint :: Grid2 () -> Position2 -> [Position2]
stepPoint g p = do
  d <- allDir4
  let p' = walk d p
  guard $ Map.notMember p' g
  pure p'

step1 :: Grid2 () -> Set Position2 -> Set Position2
step1 g = Set.fromList . concatMap (stepPoint g) . Set.toList

stepN :: Grid2 () -> Int -> Set Position2 -> Set Position2
stepN _ 0 ps = ps
stepN g n ps = stepN g (pred n) $ step1 g ps

nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge a b = nubOrd $ a ++ b

mkGarden :: Grid -> Set Position2 -> Input
mkGarden g st =
  Map.map (const Sand) (gG g) `Map.union` Map.fromSet (const Reach) st

reachableInSteps :: Position2 -> Int -> Grid2 () -> Set Position2
reachableInSteps p n g = stepN g n (Set.singleton p)

reachableInSteps1 :: Int -> Grid -> Set Position2
reachableInSteps1 n g =
  ttraceF (displayG . mkGarden g) $ reachableInSteps (Position2 0 0) n (gG g)

part1 :: Int -> Grid -> Int
part1 n = length . reachableInSteps1 n

hasOnCross :: Grid -> Bool
hasOnCross = any onCross . Map.keys . gG
  where
    onCross (Position2 0 _) = True
    onCross (Position2 _ 0) = True
    onCross _               = False

-- assuming the grid has free cross in the middle, transpose the four parts it
-- divides it into to have the original center in the corners.
--
-- A B
-- C D
--
-- becomes
--
-- DB
-- CA
--
-- It will have double free cross in the middle thanks to the original grid
-- having free borders.
transform :: Grid -> Grid2 ()
transform g = Map.mapKeys tp (gG g)
  where
    tp (Position2 x y) = Position2 (t xmax xmin x) (t ymax ymin y)
    t amax amin a
      | a < 0 = a + amax - amin + 1
      | otherwise = a
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

part2 :: Int -> Grid -> Int
part2 n g
  | hasOnCross g = ttrace "paths on cross, ignoring" 0
  | otherwise = part2' n (transform g)

metadata :: Grid -> Text
metadata g = "hasOnCross=" <> tshow (hasOnCross g) <> " size=" <> tshow (sx, sy)
  where
    Position2 sx sy = pointMinus pMax pMin
    (pMin, pMax) = gBounds g

half :: Int -> Int
half i = i `div` 2

timesPredHalf :: Int -> Int
timesPredHalf i = half $ i * pred i

timesSuccHalf :: Int -> Int
timesSuccHalf = timesPredHalf . succ

startingPoint :: Direction4 -> Grid2 () -> Position2
startingPoint d1 g =
  case d1 of
    N -> Position2 (pred xmin) ymax
    E -> Position2 xmin (pred ymin)
    S -> Position2 (succ xmax) ymin
    W -> Position2 xmax (succ ymax)
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

gridSize :: Grid2 () -> Int
gridSize g = succ $ xmax - xmin
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g

part2' :: Int -> Grid2 () -> Int
part2' n g = 1 + 4 * (fill + full) + sum (map triangles allDir4)
  where
    reach p n = length $ reachableInSteps p (traceShowF ("reach arg",) n) g
    triangles d = less * reach p dLess + more * reach p dMore
      where
        p = startingPoint d g
    r = gridSize g
    r' = succ r
    k = n `div` r'
    less = succ k
    more = k
    full = timesPredHalf k
    dLess = n - k * r'
    dMore = dLess + r'
    fill = half $ timesPredHalf (pred n) + r * r * timesSuccHalf k

tasks =
  Tasks
    2023
    21
    (CodeBlock 0)
    parser
    [ AssertExample "part 1" 16 $ part1 6
    , taskBlind (part1 64) & taskPart 1
    , Assert "part 1 test" 16 $ part1 4 testInput
    , Assert "transform" (Position2 1 1, Position2 10 10)
        $ boundsG
        $ ttraceF displayG
        $ transform testInput
    , Assert "metadata" "hasOnCross=False size=(10,10)" $ metadata testInput
    , taskBlind metadata
    -- , Assert "all on single grid" (33, 30) $ allOnGrid testInput
    -- can't apply the same algorithm to part 2 _example_ because it doesn't
    -- have the free cross in the middle
    -- , AssertExample "part 2 1" 2 $ part2 1
    -- , AssertExample "part 2 2" 4 $ part2 2
    -- , AssertExample "part 2 6" 16 $ part2 6
    -- , AssertExample "part 2 10" 50 $ part2 10
    -- , AssertExample "part 2 100" 6536 $ part2 100
    -- , AssertExample "part 2 500" 167004 $ part2 500
    -- , AssertExample "part 2 1000" 668697 $ part2 1000
    , taskBlind (part2 1)
    , taskBlind (part2 26501365) & taskPart 2
    ]
