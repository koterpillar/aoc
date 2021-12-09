module Grid where

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

data Direction4
  = E
  | N
  | W
  | S
  deriving (Enum, Eq, Ord, Show)

allDir4 :: [Direction4]
allDir4 = [E, N, W, S]

turnLeft :: Direction4 -> Direction4
turnLeft S = E
turnLeft d = succ d

turnRight :: Direction4 -> Direction4
turnRight E = S
turnRight d = pred d

reverse4 :: Direction4 -> Direction4
reverse4 = turnLeft . turnLeft

walk :: Direction4 -> Position2 -> Position2
walk = walkN 1

walkN :: Int -> Direction4 -> Position2 -> Position2
walkN n E (Position2 x y) = Position2 (x + n) y
walkN n W (Position2 x y) = Position2 (x - n) y
walkN n N (Position2 x y) = Position2 x (y - n)
walkN n S (Position2 x y) = Position2 x (y + n)
