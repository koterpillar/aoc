module Direction4 where

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
