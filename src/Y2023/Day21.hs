{-# LANGUAGE ScopedTypeVariables #-}

module Y2023.Day21 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

origin :: Position2
origin = Position2 0 0

data GardenItem
  = Sand
  | Gnome
  deriving (Ord, Eq, Show, Enum, Bounded)

type Input = Grid2 (Maybe GardenItem)

data Grid = Grid
  { gG            :: Grid2 ()
  , gSize         :: Int
  , gOriginalSize :: Int
  } deriving (Eq, Ord, Show)

gMaxReach :: Grid -> Int
gMaxReach g = (gSize g - 1) `div` 2

gContains :: Position2 -> Grid -> Bool
gContains (Position2 x y) g = abs x <= gMaxReach g && abs y <= gMaxReach g

gFree :: Position2 -> Grid -> Bool
gFree p g = Map.notMember p (gG g)

gWrap :: Grid -> Int -> Int
gWrap g = wrapRange (negate r) r
  where
    r = gMaxReach g

gFreeWrap :: Position2 -> Grid -> Bool
gFreeWrap (Position2 x y) g = gFree (Position2 (gWrap g x) (gWrap g y)) g

findGnomeCenter :: Input -> Grid
findGnomeCenter i = Grid {..}
  where
    (start, Just Gnome) =
      fromSingleE "gnome" $ Map.toList $ Map.filter (== Just Gnome) i
    t = flip pointMinus start
    gG = Map.mapKeys t $ void $ Map.filter (== Just Sand) i
    gBounds@(Position2 xmin ymin, Position2 xmax ymax) = boundsG i
    gSizeX = xmax - xmin + 1
    gSizeY = ymax - ymin + 1
    gSize
      | gSizeX == gSizeY = gSizeX
      | otherwise = error $ "non-square grid: " <> show (gSizeX, gSizeY)
    gOriginalSize = gSize

parser :: Parser Text Grid
parser =
  findGnomeCenter
    <$> charGridP'
          (choiceP [('#', Just Sand), ('.', Nothing), ('S', Just Gnome)])

testInput1 :: Grid
testInput1 =
  justParse parser
    $ Text.unlines
        [ "..........."
        , ".####.##.#."
        , ".####.#...."
        , ".####.#..#."
        , ".####.##.#."
        , ".....S....."
        , ".......#..."
        , "......####."
        , ".......#..."
        , ".......#..."
        , "..........."
        ]

testInput :: Grid
testInput =
  justParse parser
    $ Text.unlines
        [ "........."
        , ".###.#.#."
        , ".###.#.#."
        , ".#.#.###."
        , "....S...."
        , "........."
        , ".....###."
        , "........."
        , "........."
        ]

miniInput :: Grid
miniInput =
  justParse parser $ Text.unlines ["...#.", ".#...", "..S..", "...#.", "....."]

enlarge :: Int -> Grid -> Grid
enlarge n (Grid g sz osz) = Grid g' sz' osz
  where
    range = [negate n .. n]
    g' = Map.fromList $ concatMap mkP $ Map.toList g
    mkP (p, ()) = [(offset x y p, ()) | x <- range, y <- range]
    offset x y p = p `pointPlus` Position2 (x * sz) (y * sz)
    sz' = length range * sz

type PointPredicate = Position2 -> Bool

reachableInSteps :: PointPredicate -> Int -> Position2 -> Set Position2
reachableInSteps valid n = iterateNL n go . Set.singleton
  where
    go = Set.fromList . concatMap expand . Set.toList
    expand p = do
      d <- allDir4
      let p' = walk d p
      guard $ valid p'
      pure p'

reachable1 :: Grid -> Int -> Set Position2
reachable1 g n = reachableInSteps valid n origin
  where
    valid p = gContains p g && gFree p g

reachable1Trace :: Grid -> Int -> Set Position2
reachable1Trace g n = traceReach g $ reachable1 g n

part1 :: Int -> Grid -> Int
part1 n g = length $ reachable1Trace g n

displayExtra :: Grid -> Map Position2 Char -> LazyByteString
displayExtra g extra = displayPixels opts $ mkGarden g
  where
    mkGarden g = Map.map (const '#') (gG g) `Map.union` extra
    opts =
      pixelZoom 10 <> pixelCheckerSize (gOriginalSize g) <> pixelCheckerOffset 1

displayReach :: Grid -> Set Position2 -> LazyByteString
displayReach g = displayExtra g . Map.fromSet (const 'O')

displayGrid :: Grid -> LazyByteString
displayGrid g = displayExtra g Map.empty

traceReach :: Grid -> Set Position2 -> Set Position2
traceReach g = lbtraceF (displayReach g)

sameSign :: Int -> Int -> Bool
sameSign a b = a * b >= 0

type Quadrant = (Int, Int)

quadrants :: [Quadrant]
quadrants = [(qx, qy) | qx <- [-1, 1], qy <- [-1, 1]]

inQuadrant :: Quadrant -> Position2 -> Bool
inQuadrant (qx, qy) (Position2 x y) = sameSign x qx && sameSign y qy

reachableQuadrant :: Quadrant -> Grid -> Int -> Set Position2
reachableQuadrant q g n =
  Set.filter (maxDistance $ pred sz) $ reachableInSteps valid n origin
  where
    maxDistance n p =
      let Position2 dx dy = pointMinus p origin
       in abs dx <= n && abs dy <= n
    sz = gSize g
    valid p = inQuadrant q p && gFreeWrap p g && maxDistance sz p

sqr :: Int -> Int
sqr a = a * a

(*?) :: Int -> Int -> Int
0 *? _ = 0
a *? b = a * b

infixl 7 *?

normaliseParity :: Int -> Int -> Int
normaliseParity example n = n + (example - n) `mod` 2

part2Q :: Int -> Grid -> Quadrant -> Int
part2Q n g q =
  trace
    ("n="
       <> show n
       <> ", k="
       <> show k
       <> ", f1="
       <> show f1
       <> ", f2="
       <> show f2
       <> ", r1="
       <> show r1
       <> ", r2="
       <> show r2
       <> ", d1="
       <> show d1
       <> ", d2="
       <> show d2
       <> ", c1="
       <> show c1
       <> ", c2="
       <> show c2
       <> ", p1="
       <> show p1
       <> ", p2="
       <> show p2
       <> ", res="
       <> show res)
    res
  where
    res = f1 *? r1 + f2 *? r2 + p1 *? c1 + p2 *? c2
    sz = gSize g
    k = n `div` sz
    d1 = n `mod` sz
    d2 = d1 + sz
    f1 = sqr $ k `div` 2
    f2 =
      let k' = succ k `div` 2
       in k' * pred k'
    max = sz * 2
    r1 =
      length
        $ reachableQuadrant q g
        $ traceShowF ("r1 arg", )
        $ normaliseParity n max
    r2 =
      length
        $ reachableQuadrant q g
        $ traceShowF ("r2 arg", )
        $ normaliseParity (succ n) max
    c1 = length $ reachableQuadrant q g d1
    c2 = length $ reachableQuadrant q g d2
    p1 = succ k
    p2 = k
    g' = enlarge 1 g

part2 :: Int -> Grid -> Int
part2 n g =
  traceShow ("q", quadrantSum, "c", centerFix, "l", lineFix)
    $ quadrantSum - centerFix - lineFix
  where
    quadrantSum = sum $ map (part2Q n g) quadrants
    centerFix
      | even n = 3
      | otherwise = 0
    lineFix = 4 * (succ n `div` 2)

part2NaiveCheck :: Int -> Grid -> Text
part2NaiveCheck steps g
  | Map.member (Position2 1 0) (gG g) = "Example, ignoring"
  | naive == fast = ttrace ("Result = " <> tshow fast) $ "OK " <> tshow steps
  | otherwise =
    "FAIL for " <> tshow steps <> ": " <> tshow fast <> " /= " <> tshow naive
  where
    naive = length naiveSet
    naiveG = enlarge t g
    naiveSet = reachable1 naiveG steps
    fast = part2 steps g
    t = succ $ steps `div` gSize g

realSteps :: Int
realSteps = 26501365

traceGrid :: Grid -> Grid
traceGrid = lbtraceF displayGrid

tasks =
  Tasks 2023 21 (CodeBlock 0) parser
    $ [ AssertExample "part 1" 16 $ part1 6
      , taskBlind (part1 64) & taskPart 1
      , Assert "part 1 test 0" 1 $ part1 0 testInput
      , Assert "part 1 test 1" 4 $ part1 1 testInput
      , Assert "part 1 test 2" 7 $ part1 2 testInput
      , Assert "part 1 test 4" 16 $ part1 4 testInput
      , Assert "enlarge length" (25 * length (gG testInput))
          $ length
          $ gG
          $ traceGrid
          $ enlarge 2 testInput
      , AssertExample "part 1" 16 $ part1 6
      , taskBlind (part1 64) & taskPart 1
      , Assert "part 1 test 0" 1 $ part1 0 testInput
      , Assert "part 1 test 1" 4 $ part1 1 testInput
      , Assert "part 1 test 2" 7 $ part1 2 testInput
      , Assert "part 1 test 4" 16 $ part1 4 testInput
      , Assert "enlarge length" (25 * length (gG testInput))
          $ length
          $ gG
          $ traceGrid
          $ enlarge 2 testInput
      ]
        ++ [ Assert
             ("naive check test input " <> tshow i <> " steps " <> tshow steps)
             ("OK " <> tshow steps)
             (part2NaiveCheck steps input)
           | steps <- [0 .. 80] ++ [100, 101, 102, 156, 157]
           , (i, input) <- zipN 0 [testInput, testInput1]
           ]
        ++ [taskBlind (part2NaiveCheck steps) | steps <- [310, 311, 460, 461]]
        ++
    -- can't apply the same algorithm to part 2 _example_ because it doesn't
    -- have the free cross in the middle
    -- , AssertExample "part 2 1" 2 $ part2 1
    -- , AssertExample "part 2 2" 4 $ part2 2
    -- , AssertExample "part 2 6" 16 $ part2 6
    -- , AssertExample "part 2 10" 50 $ part2 10
    -- , AssertExample "part 2 100" 6536 $ part2 100
    -- , AssertExample "part 2 500" 167004 $ part2 500
    -- , AssertExample "part 2 1000" 668697 $ part2 1000
         [taskBlind (part2 realSteps) & taskPart 2]
