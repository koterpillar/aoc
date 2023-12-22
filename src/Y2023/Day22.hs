{-# LANGUAGE Strict #-}

module Y2023.Day22 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type P3 = (Int, Int, Int)

data Brick = Brick
  { brickName :: Text
  , brickA    :: P3
  , brickB    :: P3
  } deriving (Eq, Ord, Show)

brickNames :: [Text]
brickNames =
  [Text.singleton c | c <- ['A' .. 'Z']] ++ map (Text.cons '#' . tshow) [27 ..]

p3p :: Parser Text P3
p3p = tsplitP "," &* ap3P (,,) integerP integerP integerP

mkBrick :: Text -> (P3, P3) -> Brick
mkBrick n (a, b) = Brick n a b

parser :: Parser Text [Brick]
parser = zipWith mkBrick brickNames <$> linesP &** tsplitP "~" &* (p3p &+ p3p)

type BrickMap = Map P3 Text

brickMap :: [Brick] -> BrickMap
brickMap bs =
  mapFromListWith (error "multiple bricks") $ do
    (Brick name (x1, y1, z1) (x2, y2, z2)) <- bs
    x <- [x1 .. x2]
    y <- [y1 .. y2]
    z <- [z1 .. z2]
    pure ((x, y, z), name)

spaceBelow1 :: BrickMap -> Text -> P3 -> Int
spaceBelow1 bm bn (x, y, z) = z - zStop - 1
  where
    canFall z' =
      case Map.lookup (x, y, z') bm of
        Nothing  -> True
        Just bn' -> bn == bn'
    zStop = fromMaybe 0 $ find (not . canFall) [z,pred z .. 1]

spaceBelow :: BrickMap -> Text -> Int
spaceBelow bm bn =
  fromJustE
    ("spaceBelow: empty brick "
       <> show bn
       <> " in\n"
       <> Text.unpack (displayBrickMap bm))
    $ maybeMinimum
    $ [spaceBelow1 bm bn p | (p, n) <- Map.toList bm, n == bn]

fall1brick :: BrickMap -> Text -> BrickMap
fall1brick bm bn
  | dz == 0 = bm
  | otherwise =
    Map.fromList $ do
      (p@(x, y, z), n) <- Map.toList bm
      let p' =
            if n == bn
              then (x, y, z - dz)
              else p
      for_ (Map.lookup p' bm) $ \n' ->
        when (n' /= n) $ error $ show ("brick fell into another", p, n, p', n')
      pure (p', n)
  where
    dz = spaceBelow bm bn

sortedBrickNames :: BrickMap -> [Text]
sortedBrickNames = nubOrd . map snd . sortOn (\((_, _, z), _) -> negate z) . Map.toList

fall1 :: BrickMap -> BrickMap
fall1 bm = go (sortedBrickNames bm) bm
  where
    go [] bm     = bm
    go (b:bs) bm = go bs $ fall1brick bm b

fall :: BrickMap -> BrickMap
fall = iterateSettleL (traceBM . fall1)

stoppers :: BrickMap -> Map Text (Set Text)
stoppers bm =
  mapFromListS $ do
    (p@(x, y, z), b) <- Map.toList bm
    let p' = (x, y, z - 1)
    b2 <- toList $ Map.lookup p' bm
    guard $ b2 /= b
    pure (b, Set.singleton b2)

projectX :: P3 -> Position2
projectX (x, y, z) = Position2 y $ negate z

projectY :: P3 -> Position2
projectY (x, y, z) = Position2 x $ negate z

displayBrickMap :: BrickMap -> Text
displayBrickMap bm =
  Text.unlines [displayProject project bm | project <- [projectX, projectY]]
  where
    displayProject :: (P3 -> Position2) -> BrickMap -> Text
    displayProject project bm =
      displayG
        $ Map.map (mkchr . Set.toList)
        $ mapFromListS
        $ map (bimap project Set.singleton)
        $ Map.toList bm
    mkchr [b] = Text.head b
    mkchr _   = '?'

traceBM = ttraceF displayBrickMap

singles :: Ord b => Map a (Set b) -> Set b
singles = foldr1 Set.union . filter ((== 1) . length) . toList

part1 :: [Brick] -> Int
part1 bs = length bs - length bSingleStoppers
  where
    bm = traceBM $ brickMap bs
    bm1 = fall bm
    bStoppers = stoppers bm1
    bSingleStoppers = singles bStoppers

tasks =
  Tasks
    2023
    22
    (CodeBlock 0)
    parser
    [ AssertExample
        "space below"
        0
        (\bs ->
           let bm = brickMap bs
            in spaceBelow bm "B")
    , task part1 5 & taskPart 1
    ]
