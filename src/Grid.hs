module Grid where

import           Data.Map  (Map)
import qualified Data.Map  as Map

import           Data.Text (Text)
import qualified Data.Text as Text

import           Utils

data Position2 =
  Position2
    { pX :: !Int
    , pY :: !Int
    }
  deriving (Eq, Ord, Show)

manhattanDistance :: Position2 -> Position2 -> Int
manhattanDistance (Position2 x1 y1) (Position2 x2 y2) =
  abs (x2 - x1) + abs (y2 - y1)

bounds :: [Position2] -> (Position2, Position2)
bounds [] = error "No bounds for empty list"
bounds ps = (Position2 xmin ymin, Position2 xmax ymax)
  where
    xmin = minimum $ map pX ps
    ymin = minimum $ map pY ps
    xmax = maximum $ map pX ps
    ymax = maximum $ map pY ps

displayGrid :: (a -> Text) -> [[a]] -> Text
displayGrid fn = Text.unlines . map (mconcat . map fn)

data Direction4
  = E
  | N
  | W
  | S
  deriving (Enum, Eq, Ord, Show, Bounded)

allDir4 :: [Direction4]
allDir4 = [minBound .. maxBound]

turnLeft :: Direction4 -> Direction4
turnLeft S = E
turnLeft d = succ d

turnRight :: Direction4 -> Direction4
turnRight E = S
turnRight d = pred d

reverse4 :: Direction4 -> Direction4
reverse4 = turnLeft . turnLeft

data Direction8
  = E_
  | NE
  | N_
  | NW
  | W_
  | SW
  | S_
  | SE
  deriving (Eq, Ord, Show, Bounded, Enum)

allDir8 :: [Direction8]
allDir8 = [minBound .. maxBound]

walk :: Walkable2 d => d -> Position2 -> Position2
walk = walkN 1

class Walkable2 d where
  walkN :: Int -> d -> Position2 -> Position2

instance Walkable2 Direction4 where
  walkN n E (Position2 x y) = Position2 (x + n) y
  walkN n W (Position2 x y) = Position2 (x - n) y
  walkN n N (Position2 x y) = Position2 x (y - n)
  walkN n S (Position2 x y) = Position2 x (y + n)

instance Walkable2 Direction8 where
  walkN n E_ = walkN n E
  walkN n NE = walkN n N . walkN n E
  walkN n N_ = walkN n N
  walkN n NW = walkN n N . walkN n W
  walkN n W_ = walkN n W
  walkN n SW = walkN n S . walkN n W
  walkN n S_ = walkN n S
  walkN n SE = walkN n S . walkN n E

enumerate2 :: [[a]] -> Map Position2 a
enumerate2 = Map.fromList . concat . zipWith makeLine [0 ..]
  where
    makeLine y = zipWith (makePoint y) [0 ..]
    makePoint y x v = (Position2 x y, v)

mapToGrid :: Map Position2 a -> [[Maybe a]]
mapToGrid m =
  [[Map.lookup (Position2 x y) m | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = bounds $ Map.keys m
