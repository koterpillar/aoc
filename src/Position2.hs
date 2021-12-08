module Position2 where

data Position2 =
  Position2
    { pX :: !Int
    , pY :: !Int
    }
  deriving (Eq, Ord, Show)

manhattanDistance :: Position2 -> Position2 -> Int
manhattanDistance (Position2 x1 y1) (Position2 x2 y2) =
  abs (x2 - x1) + abs (y2 - y1)
