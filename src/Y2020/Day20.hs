module Y2020.Day20 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Text           as Text

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

flips :: Tile -> [Tile]
flips (Tile i g) =
  [ Tile i $ Map.mapKeys (a . b . c) g
  | a <- [id, flipX]
  , b <- [id, flipY]
  , c <- [id, flipXY]
  ]
  where
    flipX (Position2 x y) = Position2 (xmax + xmin - x) y
    flipY (Position2 x y) = Position2 x (ymax + ymin - y)
    flipXY (Position2 x y) = Position2 y x
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

edge :: Direction4 -> Tile -> Edge
edge d Tile {tgrid = g} = [Map.member p g | p <- points d]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    points E = [Position2 xmax y | y <- [ymin .. ymax]]
    points N = [Position2 x ymin | x <- [xmin .. xmax]]
    points W = [Position2 xmin y | y <- [ymin .. ymax]]
    points S = [Position2 x ymax | x <- [xmin .. xmax]]

data GridLink =
  GridLink
    { tlT :: Tile
    , tlE :: Map Direction4 Edge
    }
  deriving (Show)

tlN :: GridLink -> Edge
tlN GridLink {..} = tlE Map.! N

-- Oops, we'll have 8 edges because of flips, but can't use them all at once!
-- Thankfully the edges are all unique even with rotations.
links :: Tile -> [GridLink]
links = map go . flips
  where
    go t = GridLink t (Map.fromList [(d, edge d t) | d <- allDir4])

edges :: Tile -> [Edge]
edges = map tlN . links

allEdges :: [Tile] -> Map Edge [Int]
allEdges tiles =
  Map.fromListWith (++) [(edge, [tid t]) | t <- tiles, edge <- edges t]

cornerIds :: [Tile] -> Int
cornerIds tiles = product corners
  where
    ae = allEdges tiles
    unmatched = Map.filter ((== 1) . length) ae
    unmatchedWithCount =
      traceShowId $ mapFromListCount $ join $ Map.elems unmatched
    corners = Map.keys $ Map.filter (== 4) unmatchedWithCount -- 4 because of flips

type GridLinks = Map Edge [GridLink]

gridLinks :: [Tile] -> GridLinks
gridLinks = Map.fromListWith (++) . map mkKey . concatMap links
  where
    mkKey gl = (tlN gl, [gl])

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
