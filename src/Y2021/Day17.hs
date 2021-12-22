module Y2021.Day17 where

import           Data.Either
import qualified Data.Map    as Map
import qualified Data.Text   as Text

import           AOC
import           Grid
import           Utils

inBounds :: Ord k => k -> k -> k -> Ordering
inBounds vmin vmax v
  | v < vmin = LT
  | v > vmax = GT
  | otherwise = EQ

vxStep :: Int -> Int
vxStep 0 = 0
vxStep v
  | v < 0 = error "can't have negative x speed"
  | otherwise = v - 1

type Velocity = Position2

velocityStep :: Velocity -> Velocity
velocityStep (Position2 vx y) = Position2 (vxStep vx) (y - 1)

applyVelocity :: Velocity -> Position2 -> Position2
applyVelocity (Position2 dx dy) (Position2 x y) = Position2 (x + dx) (y + dy)

type TargetArea = (Position2, Position2)

projectilePath :: TargetArea -> Velocity -> [Position2]
projectilePath area@(Position2 _ ymin, _) v = go v (Position2 0 0)
  where
    go v p
      | insideBounds area p = [p]
      | pY p < ymin && pY v < 0 = []
      | otherwise = p : go (velocityStep v) (applyVelocity v p)

highestPoint :: TargetArea -> Velocity -> Int
highestPoint area =
  fromJustE "highestPoint" . maybeMaximum . map pY . projectilePath area

vxmin :: TargetArea -> Int
vxmin (Position2 xmin _, _) -- FIXME correct?
 =
  fromJustE ("vxmin " <> show xmin) $
  listToMaybe $ dropWhile (\v -> xres v < xmin) [0 ..]
  where
    xres v = v * (v + 1)

vxmax :: TargetArea -> Int
vxmax (_, Position2 xmax _) = xmax + 1

vymin :: TargetArea -> Int
vymin (Position2 _ ymin, _) = ymin - 1

vymax :: TargetArea -> Int
vymax area@(Position2 _ ymin, Position2 _ ymax)
  | ymax >= 0 && ymin <= 0 = error "vymax: can hit at any speed"
  | otherwise = negate $ vymin area

doesHit :: TargetArea -> Velocity -> Bool
doesHit area = insideBounds area . last . projectilePath area

hits :: TargetArea -> [Velocity]
hits area = do
  vx <- [vxmin area .. vxmax area]
  vy <- [vymin area .. vymax area]
  let v = Position2 vx vy
  guard $ doesHit area v
  pure v

part1 :: TargetArea -> Int
part1 area =
  highestPoint area $
  ttraceF (displayScene 5 area) $
  maximumBy (compare `on` highestPoint area) $ hits area

part2 :: TargetArea -> Int
part2 = length . hits

displayScene :: Int -> TargetArea -> Velocity -> Text
displayScene scale area@(Position2 xmin ymin, Position2 xmax ymax) v = display
  where
    display =
      displayG $
      shrinkWithG scale pref $
      Map.mapKeys (\(Position2 x y) -> Position2 x (-y)) $
      Map.fromList $ areaPoints ++ pathPoints
    areaPoints =
      [(Position2 x y, '#') | x <- [xmin .. xmax], y <- [ymin .. ymax]]
    pathPoints = [(p, '*') | p <- projectilePath area v]
    pref '*' '#' = '*'
    pref _ v     = v

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
    [ Task part1 45
    , AssertExample
        "8,1 hits"
        True
        (\a ->
           let v = Position2 8 1
            in ttrace (displayScene 1 a v) doesHit a v)
    , Task part2 112
    ]
