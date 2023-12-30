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
  { gG      :: Grid2 ()
  , gBounds :: (Position2, Position2)
  , gSize   :: Int
  } deriving (Eq, Ord, Show)

findGnomeCenter :: Input -> Grid
findGnomeCenter i = Grid {..}
  where
    (start, Just Gnome) =
      fromSingleE "gnome" $ Map.toList $ Map.filter (== Just Gnome) i
    t = flip pointMinus start
    gG = Map.mapKeys t $ void $ Map.filter (== Just Sand) i
    gBounds@(Position2 xmin ymin, Position2 xmax ymax) = bimap t t $ boundsG i
    gSizeX = xmax - xmin + 1
    gSizeY = ymax - ymin + 1
    gSize
      | gSizeX == gSizeY = gSizeX
      | otherwise = error $ "non-square grid: " <> show (gSizeX, gSizeY)

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
  justParse parser $ Text.unlines [".....", ".#...", "..S..", "...#.", "....."]

enlarge :: Int -> Grid -> Grid
enlarge n g = Grid g' b' s'
  where
    nmin = negate n
    nmax = n
    g' = Map.fromList $ concatMap mkP $ Map.toList $ gG g
    mkP (p, ()) =
      [(offset x y p, ()) | x <- [nmin .. nmax], y <- [nmin .. nmax]]
    offset x y p = p `pointPlus` Position2 (x * xdiff) (y * ydiff)
    (pmin, pmax) = gBounds g
    pdiff = pmax `pointMinus` pmin
    xdiff = succ $ pX pdiff
    ydiff = succ $ pY pdiff
    b' = bimap (offset nmin nmin) (offset nmax nmax) (gBounds g)
    s' = n * gSize g

stepPoint :: Grid -> Position2 -> [Position2]
stepPoint g p = do
  d <- allDir4
  let p' = walk d p
  guard $ insideBounds (gBounds g) p'
  guard $ Map.notMember p' (gG g)
  pure p'

step1 :: Grid -> Set Position2 -> Set Position2
step1 g = Set.fromList . concatMap (stepPoint g) . Set.toList

stepN :: Grid -> Int -> Set Position2 -> Set Position2
stepN _ 0 ps = ps
stepN g n ps = stepN g (pred n) $ step1 g ps

reachableFrom :: Position2 -> Int -> Grid -> Set Position2
reachableFrom p n g =
  stepN g n (Set.singleton $ traceShow ("reachable", p, n) p)

displayExtra :: Grid -> Map Position2 Char -> LazyByteString
displayExtra g extra = displayPixels 6 $ mkGarden g
  where
    mkGarden g =
      Map.map (const '#') (gG g)
        `Map.union` extra
        `Map.union` Map.fromList
                      [ (Position2 x y, 'X')
                      | x <- [xmin, xmax]
                      , y <- [ymin, ymax]
                      ]
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

displayReach :: Grid -> Set Position2 -> LazyByteString
displayReach g = displayExtra g . Map.fromSet (const 'O')

reachableFromTrace :: Position2 -> Int -> Grid -> Set Position2
reachableFromTrace p n g = lbtraceF (displayReach g) $ reachableFrom p n g

part1 :: Int -> Grid -> Int
part1 n = length . reachableFromTrace origin n

gMiddles :: Grid -> [(Position2, Int)]
gMiddles g =
  map
    (\p -> (p, manhattanDistance origin p + 1))
    [Position2 0 ymin, Position2 0 ymax, Position2 xmin 0, Position2 xmax 0]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

gCorners :: Grid -> [(Position2, Int)]
gCorners g =
  map
    (\p -> (p, manhattanDistance origin p + 2))
    [ Position2 xmin ymin
    , Position2 xmin ymax
    , Position2 xmax ymin
    , Position2 xmax ymax
    ]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

(*?) :: Int -> Int -> Int
0 *? _ = 0
x *? y = x * y

infixl 7 *?

part2 :: Int -> Grid -> Int
part2 n g =
  traceShow ("center", center, "lines", lines, "corners", corners)
    $ center + lines + corners
  where
    sz = gSize g
    k = succ n `div` sz
    toCorner = sz - 1
    maxReach = toCorner + (toCorner - n) `mod` 2
    full1 = length $ reachableFromTrace origin maxReach g
    full2 = length $ reachableFromTrace origin (succ maxReach) g
    sqr a = a * a
    k' = max 0 $ pred k
    center
      | n <= sz = length $ reachableFromTrace origin n g
      | otherwise = sqr k *? full1 + sqr k' *? full2
    lines =
      sum
        [ length
          $ reachableFromTrace
              middle
              (traceShowF ("part2Line", "n", n, "d", d, "sz", sz, "arg", )
                 $ (n - d) `mod` sz)
              g
        | (middle, d) <-  gMiddles g
        , n >= d
        ]
    corners =
      sum
        [ k *? length (reachableFromTrace corner n' g) + k' *? length (reachableFromTrace corner (n' + sz) g)
        | (corner, d) <- traceShowF ("gCorners", ) (gCorners g)
        , n >= d
        , let n' = traceShowF ("part2Corner", "n", n, "d", d, "sz", sz, "k", k, "k'", k', "arg",) $ (n - d) `mod` sz
        ]

part2NaiveCheck :: Int -> Grid -> Text
part2NaiveCheck steps g
  | Map.member (Position2 1 0) (gG g) = "Example, ignoring"
  | naive == fast = ttrace ("Result = " <> tshow fast) $ "OK " <> tshow steps
  | otherwise =
    "FAIL for " <> tshow steps <> ": " <> tshow fast <> " /= " <> tshow naive
  where
    naive = length naiveSet
    naiveSet = reachableFromTrace origin steps $ enlarge t g
    fast = part2 steps g
    t = succ $ steps `div` gSize g

realSteps :: Int
realSteps = 26501365

tasks =
  Tasks 2023 21 (CodeBlock 0) parser
    $ [ AssertExample "part 1" 16 $ part1 6
      , taskBlind (part1 64) & taskPart 1
      , Assert "part 1 test" 16 $ part1 4 testInput
      , Assert "reachable in 1 test" 4
          $ Set.size
          $ reachableFrom origin 1 testInput
      , Assert "enlarge test" (25 * length (gG testInput))
          $ length
          $ gG
          $ lbtraceF (`displayExtra` Map.empty)
          $ enlarge 2 testInput
      ]
        ++ [ Assert
             ("naive check test input " <> tshow i <> " steps " <> tshow steps)
             ("OK " <> tshow steps)
             (part2NaiveCheck steps input)
           | steps <- [0 .. 40] ++ [100, 101, 102, 156, 157]
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
