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

-- Oops, we'll have 8 edges because of flips, but can't use them all at once!
edges :: Grid -> [Edge]
edges g = map getEdge addresses
  where
    (Position2 x1 y1, Position2 x2 y2) = boundsG g
    addresses =
      withReverse $
      map
        (uncurry $ zipWith Position2)
        [ ([x1 .. x2], repeat y1)
        , ([x1 .. x2], repeat y2)
        , (repeat x1, [y1 .. y2])
        , (repeat x2, [y1 .. y2])
        ]
    withReverse = concatMap (\a -> [a, reverse a])
    getEdge = map (`Map.member` g)

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

tasks = Tasks 2020 20 (CodeBlock 0) parseTiles [Task cornerIds 20899048083289]
