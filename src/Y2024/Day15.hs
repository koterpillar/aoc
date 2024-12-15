module Y2024.Day15
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Item
  = Wall
  | Box
  | BoxLeft
  | BoxRight
  | Robot
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Item where
  showInGrid Wall     = '#'
  showInGrid Box      = 'O'
  showInGrid BoxLeft  = '['
  showInGrid BoxRight = ']'
  showInGrid Robot    = '@'

type GridOnly = Grid2 Item

type Grid = (Position2, GridOnly)

type Input = (Grid, [Direction4])

parser :: Parser Text Input
parser =
  fmap mkInput
    $ tsplitP "\n\n"
        &* (charGridP
              &+ (pureP (Text.replace "\n" "") &* charactersP &** gridItemP))
  where
    mkInput (g', m) = ((r, g), m)
      where
        r = fromJustE "robot position" $ findTuple (== Robot) $ Map.toList g'
        g = Map.delete r g'

shift :: Direction4 -> Set Position2 -> GridOnly -> GridOnly
shift d ps = Map.mapKeys go
  where
    go p
      | Set.member p ps = walk d p
      | otherwise = p

pushing :: Direction4 -> Position2 -> Item -> Maybe [Position2]
pushing d p Robot = error "Impossible: robot in the map"
pushing d p Wall = Nothing
pushing d p box =
  Just
    $ [walk d p]
        ++ [walk E p | Set.member d ns, box == BoxLeft]
        ++ [walk W p | Set.member d ns, box == BoxRight]
  where
    ns = Set.fromList [N, S]

pushedArea :: GridOnly -> Direction4 -> Position2 -> Maybe (Set Position2)
pushedArea g d p = go p Set.empty
  where
    go p acc
      | p `Set.member` acc = Just acc
      | otherwise =
        case Map.lookup p g of
          Nothing  -> Just acc
          Just obj -> pushing d p obj >>= \ps -> foldrM go (Set.insert p acc) ps

moveRobot :: Direction4 -> Grid -> Grid
moveRobot d (r, g0) =
  fromMaybe (r, g0) $ do
    let r' = walk d r
    ps <- pushedArea g0 d r'
    pure (r', shift d ps g0)

displayGrid :: Grid -> Text
displayGrid (r, g) = displayG $ Map.insert r Robot g

gps :: Grid -> Int
gps =
  sum
    . map gpsCoord
    . Map.keys
    . Map.filter (`Set.member` boxes)
    . snd
    . ttraceF displayGrid
  where
    boxes = Set.fromList [Box, BoxLeft]
    gpsCoord (Position2 x y) = x + 100 * y

part1 :: Input -> Int
part1 (g, ms) = gps $ foldl' (flip moveRobot) g ms

widen :: Grid -> Grid
widen (r, g) = (pLeft r, Map.fromList $ concatMap (uncurry go) $ Map.toList g)
  where
    pLeft (Position2 x y) = Position2 (x * 2) y
    pRight p = walk E $ pLeft p
    go p Box = [(pLeft p, BoxLeft), (pRight p, BoxRight)]
    go p x   = [(pLeft p, x), (pRight p, x)]

part2 :: Input -> Int
part2 (g, ms) = gps $ foldl' (flip moveRobot) (ttraceF displayGrid $ widen g) ms

tasks =
  Tasks
    2024
    15
    (CodeBlock 0)
    parser
    [ task part1 2028 & taskScraper (CodeBlock 1)
    , task part1 10092 & taskPart 1
    , task part2 9021 & taskPart 2
    ]
