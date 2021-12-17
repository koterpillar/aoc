module Y2021.Day17 where

import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

inBounds :: Ord k => k -> k -> k -> Ordering
inBounds vmin vmax v
  | v < vmin = LT
  | v > vmax = GT
  | otherwise = EQ

findAllHits :: Show v => (v -> Ordering) -> [v] -> [v]
findAllHits f = takeWhile (resultIs EQ) . dropWhile (resultIs LT)
  where
    resultIs ordering v = f v == ordering

findFirstHit :: Show v => (v -> Ordering) -> [v] -> Maybe v
findFirstHit f = listToMaybe . findAllHits f

vxStep :: Int -> Int
vxStep v = max 0 $ abs $ v - 1

type Velocity = Position2

velocityStep :: Velocity -> Velocity
velocityStep (Position2 vx y) = Position2 (vxStep vx) (y - 1)

applyVelocity :: Velocity -> Position2 -> Position2
applyVelocity (Position2 dx dy) (Position2 x y) = Position2 (x + dx) (y + dy)

projectilePath :: Velocity -> [Position2]
projectilePath v = go v (Position2 0 0)
  where
    go v p = p : go (velocityStep v) (applyVelocity v p)

highestPoint :: Velocity -> Int
highestPoint =
  pY .
  fst .
  fromMaybe (error "highestPoint") .
  listToMaybe . dropWhile (uncurry rising) . zipTail . projectilePath
  where
    rising (Position2 _ y1) (Position2 _ y2) = y2 > y1

type TargetArea = (Position2, Position2)

insideArea :: TargetArea -> Position2 -> Position2 -> Ordering
insideArea (Position2 xmin ymin, Position2 xmax ymax) (Position2 x1 y1) (Position2 x2 y2)
  | inBounds xmin xmax x1 == EQ && inBounds ymin ymax y1 == EQ = EQ
  | x1 > xmax = GT
  | y1 < ymax && y2 <= y1 = GT
  | otherwise = LT

vxmin :: TargetArea -> Int
vxmin (Position2 xmin _, _) =
  fromMaybe (error $ "vxmin " <> show xmin) $
  listToMaybe $ dropWhile (\v -> xres v < xmin) [0 ..]
  where
    xres v = v * (v + 1)

vxmax :: TargetArea -> Int
vxmax (_, Position2 xmax _) = xmax

vymin :: TargetArea -> Int -> Int
vymin _ _ = -20 -- FIXME

vymax :: TargetArea -> Int -> Int
vymax _ _ = 20 -- FIXME

doesHit :: TargetArea -> Velocity -> Bool
doesHit area =
  isJust .
  findFirstHit (uncurry $ insideArea area) .
  guardLength 1000 . zipTail . projectilePath

guardLength :: Show a => Int -> [a] -> [a]
guardLength _ []     = []
guardLength 0 xs     = error $ "list too long, rest: " <> show (take 10 xs)
guardLength n (x:xs) = x : guardLength (n - 1) xs

hits :: TargetArea -> [Velocity]
hits area = do
  vx <- [vxmin area .. vxmax area]
  vy <- [vymin area vx .. vymax area vx]
  let v = Position2 vx vy
  traceM $ "testing " <> show v
  guard $ doesHit area v
  traceM $ "found " <> show v
  pure v

part1 :: TargetArea -> Int
part1 =
  fromMaybe (error "No paths hitting anything found") .
  maybeMaximum . map highestPoint . hits

targetAreaP :: Parser Text TargetArea
targetAreaP =
  pureP (Text.drop (Text.length "target area: x=")) &* tsplitP ", y=" &* pairP &*
  (boundP &= boundP) &*
  pureP mkBounds
  where
    boundP :: Parser Text (Int, Int)
    boundP = tsplitP ".." &* pairP &* (integerP &= integerP)
    mkBounds ((xmin, xmax), (ymin, ymax)) =
      (Position2 xmin ymin, Position2 xmax ymax)

tasks =
  Tasks
    2021
    17
    targetAreaP
    [AssertExample "7,2 hits" True (`doesHit` Position2 7 2), Task part1 45]
