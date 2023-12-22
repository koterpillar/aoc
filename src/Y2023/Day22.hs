module Y2023.Day22 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Grid
import           Memo
import           Utils

-- Types
type P3 = (Int, Int, Int)

data Brick = Brick
  { brickName :: Text
  , brickA    :: P3
  , brickB    :: P3
  } deriving (Eq, Ord, Show)

-- Parsing
brickNames :: [Text]
brickNames =
  [Text.singleton c | c <- ['A' .. 'Z']] ++ map (Text.cons '#' . tshow) [27 ..]

p3p :: Parser Text P3
p3p = tsplitP "," &* ap3P (,,) integerP integerP integerP

mkBrick :: Text -> (P3, P3) -> Brick
mkBrick n (a, b) = Brick n a b

parser :: Parser Text [Brick]
parser = zipWith mkBrick brickNames <$> linesP &** tsplitP "~" &* (p3p &+ p3p)

-- Representation
type BrickMap = Map P3 Text

brickMap :: [Brick] -> BrickMap
brickMap bs =
  mapFromListWith (error "multiple bricks") $ do
    (Brick name (x1, y1, z1) (x2, y2, z2)) <- bs
    x <- [x1 .. x2]
    y <- [y1 .. y2]
    z <- [z1 .. z2]
    pure ((x, y, z), name)

-- Display
sideBySide :: [Text] -> Text
sideBySide ts = Text.unlines $ map Text.unwords $ transpose $ map Text.lines ts

displayBrickMap :: BrickMap -> Text
displayBrickMap bm =
  sideBySide [displayProject project bm | project <- [projectX, projectY]]
  where
    displayProject :: (P3 -> Position2) -> BrickMap -> Text
    displayProject project bm =
      displayG
        $ Map.map (displayBricks . Set.toList)
        $ mapFromListS
        $ map (bimap project Set.singleton)
        $ Map.toList bm
    displayBricks [b] = Text.head b
    displayBricks _   = '?'
    k = max 1 $ length bm `div` 500
    projectX (x, y, z) = Position2 y $ negate z `div` k
    projectY (x, y, z) = Position2 x $ negate z `div` k

traceBM :: BrickMap -> BrickMap
traceBM = ttraceF displayBrickMap

-- Fall
-- For a particular point of a brick, what will stop its fall and how far away?
supports1 ::
     BrickMap
  -> Text -- brick name the point belongs to
  -> P3 -- point
  -> (Maybe Text, Int) -- brick that will stop it (or ground if Nothing) and distance to it
supports1 m n p@(x, y, z) =
  second (\z' -> z - z' - 1)
    $ fromMaybe (Nothing, 0)
    $ listToMaybe
    $ [ (Just n', z')
      | z' <- [pred z,pred (pred z) .. 1]
      , n' <- toList $ Map.lookup (x, y, z') m
      , n' /= n
      ]

-- For a particular brick, what will stop its fall at every point and how far away?
supports :: BrickMap -> Text -> Map (Maybe Text) Int
supports m n =
  mapFromListWith min $ map (supports1 m n) $ Map.keys $ Map.filter (== n) m

type SD a = State (Map Text Int) a

fall :: [Text] -> BrickMap -> BrickMap
fall ns m = evalState (shiftAll m) Map.empty
  where
    s = Map.fromList [(n, supports m n) | n <- ns]
    totalFall :: Text -> SD Int
    totalFall n =
      gets (Map.lookup n) >>= \case
        Just r -> pure r
        Nothing -> do
          r <- minimum <$> traverse tf1 (Map.toList $ mapLookupE "fall" n s)
          modify $ Map.insert n r
          pure r
    tf1 :: (Maybe Text, Int) -> SD Int
    tf1 (Nothing, d) = pure d
    tf1 (Just n, d)  = (+) d <$> totalFall n
    shiftAll = fmap Map.fromList . traverse shift . Map.toList
    shift :: (P3, Text) -> SD (P3, Text)
    shift ((x, y, z), n) = do
      dz <- totalFall n
      pure ((x, y, z - dz), n)

-- Dependencies
stoppers :: BrickMap -> Map Text (Set Text)
stoppers bm =
  mapFromListS $ do
    (p@(x, y, z), b) <- Map.toList bm
    let p' = (x, y, z - 1)
    b2 <- toList $ Map.lookup p' bm
    guard $ b2 /= b
    pure (b, Set.singleton b2)

singles :: Ord b => Map a (Set b) -> Set b
singles = foldr1 Set.union . filter ((== 1) . length) . toList

singlePointsOfFailure ::
     (Ord a, Hashable a, Show a) => [a] -> Map a (Set a) -> Map a a
singlePointsOfFailure names deps =
  Map.fromList [(n, d) | n <- names, d <- toList $ go n]
  where
    go =
      unsafeMemo $ \a ->
        case Set.toList <$> Map.lookup a deps of
          Nothing -> Nothing -- root
          Just [dep] -> Just dep -- single point of failure found!
          Just deps -- multiple immediate dependencies
           ->
            case nubOrd $ map go deps of
              [Just dep] -> Just dep -- all point to the same one!
              _          -> Nothing -- multiple roots, no single point of failure

totalDependencies :: (Ord a, Hashable a, Show a) => [a] -> Map a a -> Map a Int
totalDependencies names spof =
  traceShow rdeps
    $ traceShowF (("totalDeps", ) . Map.filter (> 0))
    $ Map.fromList [(n, go n) | n <- names]
  where
    go n = sum (map go ns) + length ns
      where
        ns = toList $ Map.findWithDefault Set.empty n rdeps
    rdeps = mapFromListS [(d, Set.singleton n) | (n, d) <- Map.toList spof]

-- Main
part1 :: [Brick] -> Int
part1 bs = length bs - length (singles deps)
  where
    bm = traceBM $ brickMap bs
    ns = map brickName bs
    bm1 = traceBM $ fall ns bm
    deps = stoppers bm1

part2 :: [Brick] -> Int
part2 bs = sum tdeps
  where
    bm = traceBM $ brickMap bs
    ns = map brickName bs
    bm1 = fall ns bm
    deps = stoppers bm1
    names = map brickName bs
    spof = traceShowF ("spof", ) $ singlePointsOfFailure names deps
    tdeps = totalDependencies names spof

tasks =
  Tasks
    2023
    22
    (CodeBlock 0)
    parser
    [ task part1 5 & taskPart 1
    , task part2 7 & taskPart 2
    ]
