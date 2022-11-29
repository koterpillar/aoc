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
  deriving (Eq, Ord)

instance Show Tile where
  show Tile {..} = "Tile " ++ show tid ++ ":\n" ++ Text.unpack (displayG tgrid)

parseTiles :: Parser Text [Tile]
parseTiles = lineGroupsP &* pureP (filter $ not . null) &** parseTile

parseTile :: Parser [Text] Tile
parseTile =
  uncurry Tile <$>
  unconsP &*
  (pureP (Text.replace "Tile " "" . Text.replace ":" "") &* integerP &=
   pureP Text.unlines &*
   dotGridP)

newtype Edge =
  Edge [Bool]
  deriving (Eq, Ord)

instance Show Edge where
  show (Edge bs) =
    map
      (showInGridMaybe . \b ->
         if b
           then Just ()
           else Nothing)
      bs

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
edge d Tile {tgrid = g} = Edge [Map.member p g | p <- points d]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    points E = [Position2 xmax y | y <- [ymin .. ymax]]
    points N = [Position2 x ymin | x <- [xmin .. xmax]]
    points W = [Position2 xmin y | y <- [ymin .. ymax]]
    points S = [Position2 x ymax | x <- [xmin .. xmax]]

data GridLink =
  GridLink
    { glT :: Tile
    , glE :: Map Direction4 Edge
    }
  deriving (Eq, Ord, Show)

glD :: Direction4 -> GridLink -> Edge
glD d GridLink {..} = glE Map.! d

-- Oops, we'll have 8 edges because of flips, but can't use them all at once!
-- Thankfully the edges are all unique even with rotations.
links :: Tile -> [GridLink]
links = map go . flips
  where
    go t = GridLink t (Map.fromList [(d, edge d t) | d <- allDir4])

edges :: Tile -> [Edge]
edges = map (glD N) . links

allEdges :: [Tile] -> Map Edge [Int]
allEdges tiles =
  Map.fromListWith (++) [(edge, [tid t]) | t <- tiles, edge <- edges t]

findCorners :: [Tile] -> [Int]
findCorners tiles = Map.keys $ Map.filter (== 4) unmatchedWithCount -- 4 because of flips
  where
    hasCid cid Tile {..} = cid == tid
    ae = allEdges tiles
    unmatched = Map.filter ((== 1) . length) ae
    unmatchedWithCount =
      traceShowId $ mapFromListCount $ join $ Map.elems unmatched

type GridLinks = Map (Direction4, Edge) [GridLink]

gridLinks :: [Tile] -> GridLinks
gridLinks ts = Map.fromListWith (++) $ concatMap mkKey $ concatMap links ts
  where
    mkKey gl = [((d, glD d gl), [gl]) | d <- allDir4]

findLink :: Direction4 -> Edge -> State GridLinks (Maybe GridLink)
findLink d e = do
  let k = (d, e)
  ls <- gets (fromMaybe [] . Map.lookup k)
  case ls of
    []  -> pure Nothing
    [l] -> modify (Map.delete k) >> pure (Just l)
    ls  -> error $ "Multiple grids found: " ++ unwords (map show ls)

findUnmatched :: Int -> Direction4 -> State GridLinks (Set GridLink)
findUnmatched cid d =
  gets $
  setFromList .
  filter (\l -> tid (glT l) == cid) .
  concatMap snd . filter (\((k, _), v) -> k == d && length v == 1) . Map.toList

deleteTile :: Tile -> State GridLinks ()
deleteTile (Tile i _) =
  modify $ Map.mapMaybe $ removeEmpty . filter ((/= i) . tid . glT)
  where
    removeEmpty [] = Nothing
    removeEmpty ls = Just ls

findNW :: Int -> State GridLinks GridLink
findNW cid = do
  n <- findUnmatched cid N
  w <- findUnmatched cid W
  l <-
    case toList $ setIntersection n w of
      [l, l']
        | tid (glT l) == tid (glT l') -> pure l -- found two because they are transposed
      ls -> error $ "findNW: multiple links found: " ++ show ls
  deleteTile (glT l)
  pure l

findAllInDirection :: Direction4 -> GridLink -> State GridLinks [GridLink]
findAllInDirection d l = do
  next <- findLink d (glD d l)
  case next of
    Nothing -> pure []
    Just l' -> do
      rest <- findAllInDirection d l'
      pure (l' : rest)

merge1 :: [Tile] -> [[Tile]]
merge1 ts = evalState findFirst links
  where
    corner1 = headE "corner1: no corners" $ findCorners ts
    links = gridLinks ts
    findFirst = do
      s <- get
      nw <- findNW corner1
      traceShowM nw
      t2 <- findAllInDirection S nw
      pure [map glT (nw : t2)] -- FIXME: continue search

shrink :: Tile -> Tile
shrink = error "shrink: not implemented"

joinTiles :: [[Tile]] -> Grid
joinTiles = error "joinTiles: not implemented" . map (map tgrid)

mergeIds :: [Tile] -> [[Int]]
mergeIds = map (map tid) . merge1

merge :: [Tile] -> Grid
merge = joinTiles . map (map shrink) . merge1

exampleMergedIds :: [[Int]]
exampleMergedIds = [[1951, 2311, 3079], [2729, 1427, 2473], [2971, 1489, 1171]]

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
    [ Task (product . findCorners) 20899048083289
    , Task mergeIds exampleMergedIds
    , Task (displayG . merge) exampleMerged
    ]
