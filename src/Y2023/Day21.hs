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
reachableFrom p n g =
  stepN g n (Set.singleton $ traceShow ("reachable", p, n) p)

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

reachableFromTrace :: Position2 -> Int -> Grid -> Set Position2
reachableFromTrace p n g = ttraceF (displayReach g) $ reachableFrom p n g

part1 :: Int -> Grid -> Int
part1 n = length . reachableFromTrace origin n

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

assert :: (Eq a, Show a) => Text -> a -> a -> a
assert msg expected actual
  | expected == actual = actual
  | otherwise =
    error
      $ "assert: " <> show msg <> ": " <> show expected <> " /= " <> show actual

part2Line :: Position2 -> Int -> Grid -> Int
part2Line middle n g = fullCount + remainderCount
  where
    n0 = n - distanceToOutside
    sz = gSize g
    maxReach = sz * 2 -- might be less but this is safer and easier
    full = max 0 $ (n0 - maxReach) `div` sz + 1
    remainders = downSteps sz $ n0 - full * sz
    fullCount = full *? reachN middle maxReach
    remainderCount = sum [reachN middle r | r <- remainders]
    reachN p r = length $ reachableFrom p r g
    distanceToOutside = succ $ pX $ snd $ gBounds g

part2Corner :: Position2 -> Int -> Grid -> Int
part2Corner corner n g = fullCount + remainderCount
  where
    n0 = n - 2 * distanceToOutside
    sz = gSize g
    maxReach = sz * 2 -- might be less but this is safer and easier
    full = max 0 $ (n0 - maxReach) `div` sz + 1
    remainders = downSteps sz $ n0 - full * sz
    fullCount = (full * succ full) `div` 2 *? reachN corner maxReach
    remainderCount =
      sum [k * reachN corner r | (k, r) <- zipN (succ full) remainders]
    reachN p r = length $ reachableFrom p r g
    distanceToOutside = succ $ pX $ snd $ gBounds g

(*?) :: Int -> Int -> Int
0 *? _ = 0
x *? y = x * y

infixl 7 *?

part2 :: Int -> Grid -> Int
part2 n g = center + lines + corners
  where
    center = length $ reachableFrom origin (min n $ gSize g * 2) g
    -- lines = sum [part2Line middle n g | middle <- gMiddles g]
    -- corners = sum [part2Corner corner n g | corner <- gCorners g]
    lines = part2Line (Position2 xmin 0) n g where (Position2 xmin _, _) = gBounds g
    corners = 0

part2NaiveCheck :: Int -> Grid -> Text
part2NaiveCheck steps g
  | Map.member (Position2 1 0) (gG g) = "Example, ignoring"
  | naive == fast = ttrace ("Result = " <> tshow fast) $ "OK " <> tshow steps
  | otherwise =
    terror
      $ "FAIL for "
          <> tshow steps
          <> ": "
          <> tshow fast
          <> " /= "
          <> tshow naive
  where
    naive = length naiveSet
    naiveSet = reachableFrom origin steps $ enlarge t g
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
          $ ttraceF (`displayExtra` Map.empty)
          $ enlarge 2 testInput
      ]
        ++ [ Assert
             ("naive check test input " <> tshow steps)
             ("OK " <> tshow steps)
             (part2NaiveCheck steps testInput)
           | steps <- [0 .. 40] ++ [100, 101, 102, 156, 157]
           ]
        ++ [ taskBlind (part2NaiveCheck steps)
           | steps <- [1 .. 250]
           ]
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
