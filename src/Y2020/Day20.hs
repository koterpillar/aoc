module Y2020.Day20 where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

data Tile =
  Tile
    { tid   :: Int
    , tgrid :: Grid
    }
  deriving (Show)

parseTiles :: Parser Text [Tile]
parseTiles = lineGroupsP &* pureP (filter $ not . null) &** parseTile

parseTile :: Parser [Text] Tile
parseTile =
  uncurry Tile <$>
  unconsP &*
  (pureP (Text.replace "Tile " "" . Text.replace ":" "") &* integerP &=
   pureP Text.unlines &*
   dotGridP)

type Edge = [Bool]

flips :: Grid -> [Grid]
flips g =
  [ Map.mapKeys (a . b . c) g
  | a <- [id, flipX]
  , b <- [id, flipY]
  , c <- [id, flipXY]
  ]
  where
    flipX (Position2 x y) = Position2 (xmax + xmin - x) y
    flipY (Position2 x y) = Position2 x (ymax + ymin - y)
    flipXY (Position2 x y) = Position2 y x
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

edge :: Direction4 -> Grid -> Edge
edge d g = [Map.member p g | p <- points d]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    points E = [Position2 xmax y | y <- [ymin .. ymax]]
    points N = [Position2 x ymin | x <- [xmin .. xmax]]
    points W = [Position2 xmin y | y <- [ymin .. ymax]]
    points S = [Position2 x ymax | x <- [xmin .. xmax]]

-- Oops, we'll have 8 edges because of flips, but can't use them all at once!
edges :: Grid -> [Edge]
edges = map (edge N) . flips

allEdges :: [Tile] -> Map Edge [Int]
allEdges tiles =
  Map.fromListWith
    (++)
    [(edge, [tid]) | Tile {..} <- tiles, edge <- edges tgrid]

cornerIds :: [Tile] -> Int
cornerIds tiles = product corners
  where
    ae = allEdges tiles
    unmatched = Map.filter ((== 1) . length) ae
    unmatchedWithCount =
      traceShowId $ mapFromListCount $ join $ Map.elems unmatched
    corners = Map.keys $ Map.filter (== 4) unmatchedWithCount -- 4 because of flips

merge :: [Tile] -> Grid
merge = error "not implemented"

exampleMerged :: Text
exampleMerged =
  Text.replace "." (Text.singleton middleDot) $
  Text.unlines
    [ ".#.#..#.##...#.##..#####"
    , "###....#.#....#..#......"
    , "##.##.###.#.#..######..."
    , "###.#####...#.#####.#..#"
    , "##.#....#.##.####...#.##"
    , "...########.#....#####.#"
    , "....#..#...##..#.#.###.."
    , ".####...#..#.....#......"
    , "#..#.##..#..###.#.##...."
    , "#.####..#.####.#.#.###.."
    , "###.#.#...#.######.#..##"
    , "#.####....##..########.#"
    , "##..##.#...#...#.#.#.#.."
    , "...#..#..#.#.##..###.###"
    , ".#.#....#.##.#...###.##."
    , "###.#...#..#.##.######.."
    , ".#.#.###.##.##.#..#.##.."
    , ".####.###.#...###.#..#.#"
    , "..#.#..#..#.#.#.####.###"
    , "#..####...#.#.#.###.###."
    , "#####..#####...###....##"
    , "#.##..#..#...#..####...#"
    , ".#.###..##..##..####.##."
    , "...###...##...#...#..###"
    ]

tasks :: Tasks
tasks =
  Tasks
    2020
    20
    (CodeBlock 0)
    parseTiles
    [Task cornerIds 20899048083289, Task (displayG . merge) exampleMerged]
