module Utils where

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

data Direction4
  = E
  | N
  | W
  | S
  deriving (Enum, Eq, Ord, Show)

turnLeft :: Direction4 -> Direction4
turnLeft S = E
turnLeft d = succ d

data Position2 = Position2
  { pX :: Int
  , pY :: Int
  } deriving (Eq, Ord, Show)

walk :: Direction4 -> Position2 -> Position2
walk E (Position2 x y) = Position2 (x + 1) y
walk W (Position2 x y) = Position2 (x - 1) y
walk N (Position2 x y) = Position2 x (y - 1)
walk S (Position2 x y) = Position2 x (y + 1)

manhattanDistance :: Position2 -> Position2 -> Int
manhattanDistance (Position2 x1 y1) (Position2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile continue fn v
  | continue v =
    let v' = fn v
    in v : iterateWhile continue fn v'
  | otherwise = []

pad :: Int -> String -> String
pad sz str = replicate (sz - length str) ' ' ++ str

sset :: Int -> a -> [a] -> [a]
sset idx val lst = take idx lst ++ [val] ++ drop (idx + 1) lst

sremove :: Int -> [a] -> (a, [a])
sremove idx lst = (lst !! idx, take idx lst ++ drop (idx + 1) lst)

sinsert :: Int -> a -> [a] -> [a]
sinsert idx val lst = take idx lst ++ [val] ++ drop idx lst
