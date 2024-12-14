{-# LANGUAGE ScopedTypeVariables #-}

module Y2023.Day22
  ( tasks
  ) where

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
  [Text.singleton c | c <- ['A' .. 'Y']] ++ map (Text.cons 'Z' . tshow) [27 ..]

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

-- Make all the bricks fall down! In one step!
fall :: BrickMap -> BrickMap
fall m = stateMemo go (`shiftAll` m)
  where
    ns = nubOrd $ Map.elems m
    s = Map.fromList [(n, supports m n) | n <- ns]
    go rgo n =
      fmap minimum
        $ for (Map.toList $ mapLookupE "fall" n s)
        $ \case
            (Nothing, d) -> pure d
            (Just n, d) -> (+) d <$> rgo n
    shiftAll tf = fmap Map.fromList . traverse (shift tf) . Map.toList
    shift tf ((x, y, z), n) = do
      dz <- tf n
      pure ((x, y, z - dz), n)

-- Dependencies
-- Which bricks does each brick rest on?
dependencies :: BrickMap -> Map Text (Set Text)
dependencies bm =
  mapFromListS $ do
    (p@(x, y, z), b) <- Map.toList bm
    let p' = (x, y, pred z)
    b2 <- toList $ Map.lookup p' bm
    guard $ b2 /= b
    pure (b, Set.singleton b2)

dotDepLine :: Text -> Set Text -> Text
dotDepLine a bs = a <> "->" <> Text.intercalate "," (Set.toList bs)

dotGraph :: [Text] -> Text
dotGraph ls = "digraph {" <> Text.intercalate ";" ls <> "}"

dotDependencies :: Map Text (Set Text) -> Text
dotDependencies m = dotGraph $ ds ++ fs
  where
    ds = map (uncurry dotDepLine) $ Map.toList m
    fs =
      [ dotDepLine a bs <> "[color=red]"
      | (a, bs) <- Map.toList $ singlePointsOfFailure m
      ]

-- Which bricks are the only ones supporting something?
singles :: Ord b => Map a (Set b) -> Set b
singles = foldr1 Set.union . filter ((== 1) . length) . toList

-- Which bricks will cause each brick to fail if they are removed
singlePointsOfFailure ::
     forall a. (Ord a, Hashable a, Show a)
  => Map a (Set a)
  -> Map a (Set a)
singlePointsOfFailure deps =
  stateMemo go $ \x ->
    fmap mapFromListS
      $ for names
      $ \n -> do
          d <- x n
          pure (n, d)
  where
    names = Map.keys deps
    go :: Monad m => (a -> m (Set a)) -> a -> m (Set a)
    go rgo a =
      case Set.toList <$> Map.lookup a deps of
        Nothing -> pure Set.empty
        Just deps ->
          insertIfSingle deps . foldr1 Set.intersection <$> traverse rgo deps
    insertIfSingle [x] = Set.insert x
    insertIfSingle _   = id

-- Main
part1 :: [Brick] -> Int
part1 bs = length bs - length (singles deps)
  where
    bm = traceBM $ brickMap bs
    bm1 = traceBM $ fall bm
    deps = dependencies bm1

part2 :: [Brick] -> Int
part2 bs = ttrace (dotDependencies deps) $ sum $ fmap Set.size spof
  where
    bm = brickMap bs
    bm1 = fall bm
    deps = dependencies bm1
    spof = singlePointsOfFailure deps

tasks =
  Tasks
    2023
    22
    (CodeBlock 0)
    parser
    [task part1 5 & taskPart 1, task part2 7 & taskPart 2]
