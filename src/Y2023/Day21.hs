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

testInput :: Grid
testInput =
  justParse parser
    $ Text.unlines
        [ "..........."
        , ".####.##.#."
        , ".####.#...."
        , ".####.#..#."
        , ".####.####."
        , ".....S....."
        , ".......#..."
        , "......####."
        , ".......#..."
        , ".......#..."
        , "..........."
        ]

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
reachableFrom p n g = stepN g n (Set.singleton p)

displayExtra :: Grid -> Map Position2 Char -> Text
displayExtra g extra =
  "extra=" <> tshow (Map.size extra) <> "\n" <> displayG (mkGarden g)
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

displayReach :: Grid -> Set Position2 -> Text
displayReach g = displayExtra g . Map.fromSet (const 'O')

reachableInSteps1 :: Int -> Grid -> Set Position2
reachableInSteps1 n g = ttraceF (displayReach g) $ reachableFrom origin n g

part1 :: Int -> Grid -> Int
part1 n = length . reachableInSteps1 n

iterateReach :: Position2 -> Grid -> [Int]
iterateReach p g = whileDiffers2 [length $ reachableFrom p n g | n <- [0 ..]]

whileDiffers2 :: [Int] -> [Int]
whileDiffers2 (x:rest@(y:z:_))
  | x == z = [x, y]
  | otherwise = x : whileDiffers2 rest
whileDiffers2 xs = error $ "whileDiffers2: finite list: " <> show xs

possibleReach :: Position2 -> Int -> Grid -> Set Position2
possibleReach p n g =
  Set.fromList
    [ p `pointPlus` Position2 x y
    | x <- [negate n .. n]
    , y <- [negate (n - x),negate (n - x) + 2 .. n - x]
    , abs x + abs y <= n
    , insideBounds (gBounds g) (p `pointPlus` Position2 x y)
    , Map.notMember (Position2 x y) (gG g)
    ]
  where
    (Position2 x0 y0) = p

part2Naive :: Int -> Grid -> Int
part2Naive steps g = length $ reachableInSteps1 steps $ enlarge n g
  where
    n = succ $ steps `div` gSize g

gMiddles :: Grid -> [Position2]
gMiddles g =
  [Position2 0 ymin, Position2 0 ymax, Position2 xmin 0, Position2 xmax 0]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

gCorners :: Grid -> [Position2]
gCorners g =
  [ Position2 xmin ymin
  , Position2 xmin ymax
  , Position2 xmax ymin
  , Position2 xmax ymax
  ]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

downSteps :: Int -> Int -> [Int]
downSteps step start = [start,start - step .. 0]

part2 :: Int -> Grid -> Int
part2 n g = center + lines + corners
  where
    center = traceShowF ("center", ) $ length $ reachableFrom origin n g
    distanceToOutside = succ $ pX $ snd $ gBounds g
    lineReach = downSteps (gSize g) (n - distanceToOutside)
    lines =
      traceShowF ("lines", )
        $ sum [length $ reachableFrom p r g | r <- lineReach, p <- gMiddles g]
    cornerReach = zipN 1 $ downSteps (gSize g) (n - 2 * distanceToOutside)
    corners =
      traceShowF ("corners", )
        $ sum
            [ k * length (reachableFrom p r g)
            | (k, r) <- cornerReach
            , p <- gCorners g
            ]

realSteps :: Int
realSteps = 26501365

tasks =
  Tasks
    2023
    21
    (CodeBlock 0)
    parser
    [ AssertExample "part 1" 16 $ part1 6
    , taskBlind (part1 64) & taskPart 1
    , Assert "part 1 test" 16 $ part1 4 testInput
    , Assert "reachable in 1 test" 4 $ Set.size $ reachableInSteps1 1 testInput
    , Assert "iterate reach" [1, 4, 7, 10, 16, 20, 27, 30, 36, 42, 43, 44, 44]
        $ iterateReach origin testInput
    , Assert "enlarge test" (25 * length (gG testInput))
        $ length
        $ gG
        $ ttraceF (`displayExtra` Map.empty)
        $ enlarge 2 testInput
    , let steps = 157
       in Assert
            "same as enlarge"
            (part2Naive steps testInput)
            (part2 steps testInput)
    -- can't apply the same algorithm to part 2 _example_ because it doesn't
    -- have the free cross in the middle
    -- , AssertExample "part 2 1" 2 $ part2 1
    -- , AssertExample "part 2 2" 4 $ part2 2
    -- , AssertExample "part 2 6" 16 $ part2 6
    -- , AssertExample "part 2 10" 50 $ part2 10
    -- , AssertExample "part 2 100" 6536 $ part2 100
    -- , AssertExample "part 2 500" 167004 $ part2 500
    -- , AssertExample "part 2 1000" 668697 $ part2 1000
    -- , taskBlind (part2 1)
    -- , taskBlind (part2 realSteps) & taskPart 2
    ]
