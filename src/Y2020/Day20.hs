module Y2020.Day20
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text

import           AOC
import           Grid
import           Utils

mapmap :: (a -> b) -> [[a]] -> [[b]]
mapmap = map . map

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

glId :: GridLink -> Int
glId = tid . glT

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
    unmatchedWithCount = mapFromListCount $ join $ Map.elems unmatched

type GridLinks = Map (Direction4, Edge) [GridLink]

showGL :: GridLinks -> Text
showGL gls =
  Text.unlines
    [ Text.unwords [tshow d, tshow e, tshow (map glId gs)]
    | ((d, e), gs) <- Map.toList gls
    ]

showGLd :: Direction4 -> GridLinks -> Text
showGLd d' = showGL . Map.filterWithKey (\(d, _) _ -> d == d')

gridLinks :: [Tile] -> GridLinks
gridLinks ts = Map.fromListWith (++) $ concatMap mkKey $ concatMap links ts
  where
    mkKey gl = [((d, glD d gl), [gl]) | d <- allDir4]

findLink :: Direction4 -> GridLink -> State GridLinks (Maybe GridLink)
findLink d gl = do
  let e = glD (reverse4 d) gl
  let k = (d, e)
  ls <- gets (fromMaybe [] . Map.lookup k)
  case ls of
    []  -> pure Nothing
    [l] -> Just <$> deleteTile l
    ls  -> error $ "Multiple grids found: " ++ unwords (map show ls)

findUnmatched :: Int -> Direction4 -> State GridLinks (Set GridLink)
findUnmatched cid d =
  gets $
  setFromList .
  filter (\l -> glId l == cid) .
  concatMap snd . filter (\((k, _), v) -> k == d && length v == 1) . Map.toList

deleteTile :: GridLink -> State GridLinks GridLink
deleteTile l@(GridLink (Tile i _) _) = do
  modify $ Map.mapMaybe $ removeEmpty . filter ((/= i) . glId)
  pure l
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
        | glId l == glId l' -> pure l -- found two because they are transposed
      ls -> error $ "findNW: multiple links found: " ++ show ls
  deleteTile l

findStrip :: GridLink -> State GridLinks [GridLink]
findStrip l = do
  next <- findLink N l
  case next of
    Nothing -> pure []
    Just l' -> do
      rest <- findStrip l'
      pure (l' : rest)

findFirstStrip :: Int -> State GridLinks [GridLink]
findFirstStrip cid = do
  l1 <- findNW cid
  strip1 <- findStrip l1
  pure (l1 : strip1)

findNextNW :: [GridLink] -> State GridLinks (Maybe GridLink)
findNextNW [] = error "findNextNW: empty list"
findNextNW (l:_) = do
  next <- findLink W l
  case next of
    Nothing -> pure Nothing
    Just l' -> do
      deleteTile l'
      pure (Just l')

findNextStrip :: [GridLink] -> State GridLinks (Maybe [GridLink])
findNextStrip ls = do
  nw <- findNextNW ls
  case nw of
    Just l  -> Just . (l :) <$> findStrip l
    Nothing -> pure Nothing

findAllStrips :: [GridLink] -> State GridLinks [[GridLink]]
findAllStrips s1 = do
  s2' <- findNextStrip s1
  case s2' of
    Nothing -> pure [s1]
    Just s2 -> (s1 :) <$> findAllStrips s2

findAll :: Int -> State GridLinks [[GridLink]]
findAll cid = do
  strip1 <- findFirstStrip cid
  findAllStrips strip1

merge1 :: [Tile] -> [[Tile]]
merge1 ts = mapmap glT $ evalState (findAll corner1) links
  where
    corner1 = headE "corner1: no corners" $ findCorners ts
    links = gridLinks ts

shrink :: Tile -> Tile
shrink (Tile i g) = Tile i $ Map.filterWithKey (\p _ -> inner p) g
  where
    inner (Position2 x y) = x > xmin && x < xmax && y > ymin && y < ymax
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

joinTiles :: [[Grid]] -> Grid
joinTiles gss =
  Map.unions [shift x y g | (x, gs) <- zip stops gss, (y, g) <- zip stops gs]
  where
    stops = [0,8 ..]
    shift x y = Map.mapKeys (pointPlus (Position2 x y))

mergeIds :: [Tile] -> [[Int]]
mergeIds = mapmap tid . merge1

merge :: [Tile] -> Grid
merge = joinTiles . mapmap (tgrid . shrink) . merge1

fixupExample :: [[a]] -> [[a]]
fixupExample = transpose . reverse . map reverse

fixupExampleGrid :: Grid -> Grid
fixupExampleGrid =
  Map.mapMaybe id . fromMatrixG . transpose . fixupExample . toMatrixG

exampleMergedIds :: [[Int]]
exampleMergedIds = [[1951, 2311, 3079], [2729, 1427, 2473], [2971, 1489, 1171]]

exampleMerged :: Text
exampleMerged =
  Text.replace "." (Text.singleton middleDot) $
  Text.unlines $
  map
    Text.pack
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

tileSize :: Position2
tileSize = Position2 9 9

findTileSize :: [Tile] -> Position2
findTileSize ts = pmax `pointMinus` pmin
  where
    (pmin, pmax) = boundsG $ tgrid $ headE "findTileSize: empty list" ts

tasks :: Tasks
tasks =
  Tasks
    2020
    20
    (CodeBlock 0)
    parseTiles
    [ Task (product . findCorners) 20899048083289
    , Task findTileSize tileSize
    , Task (fixupExample . mergeIds) exampleMergedIds
    , Task (displayG . fixupExampleGrid . merge) exampleMerged
    ]
