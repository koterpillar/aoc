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
    b' =
      traceShowF (gBounds g, )
        $ bimap (offset nmin nmin) (offset nmax nmax) (gBounds g)
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

displayReach :: Grid -> Set Position2 -> Set Position2 -> Text
displayReach g range reach =
  "range="
    <> tshow (Set.size range)
    <> " reach="
    <> tshow (Set.size reach)
    <> "\n"
    <> displayG (mkGarden g)
  where
    mkGarden g =
      Map.map (const '#') (gG g)
        `Map.union` Map.fromSet (const 'o') range
        `Map.union` Map.fromSet (const 'O') reach
        `Map.union` Map.fromList
                      [ (Position2 x y, 'X')
                      | x <- [xmin, xmax]
                      , y <- [ymin, ymax]
                      ]
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

reachableInSteps1 :: Int -> Grid -> Set Position2
reachableInSteps1 n g =
  ttraceF (displayReach g Set.empty) $ reachableFrom origin n g

part1 :: Int -> Grid -> Int
part1 n = length . reachableInSteps1 n

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
    n = steps `div` gSize g

part2 :: Int -> Grid -> Int
part2 n g = error "part2"

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
    , Assert "enlarge test" (25 * length (gG testInput))
        $ length
        $ gG
        $ ttraceF (\g -> displayReach g Set.empty Set.empty)
        $ enlarge 2 testInput
    , let steps = 23
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
    , taskBlind (part2 1)
    , taskBlind (part2 realSteps) & taskPart 2
    ]
