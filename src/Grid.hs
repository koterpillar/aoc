module Grid where

import           Data.Hashable (Hashable (..))

import qualified Data.Map      as Map
import qualified Data.Text     as Text

import           Utils

data Position2 =
  Position2
    { pX :: !Int
    , pY :: !Int
    }
  deriving (Eq, Ord, Show)

instance Hashable Position2 where
  hashWithSalt s (Position2 x y) = hashWithSalt s (x, y)

pointM :: Int -> Position2 -> Position2
pointM m (Position2 x y) = Position2 (m * x) (m * y)

pointPlus :: Position2 -> Position2 -> Position2
pointPlus (Position2 x1 y1) (Position2 x2 y2) = Position2 (x1 + x2) (y1 + y2)

pointMinus :: Position2 -> Position2 -> Position2
pointMinus (Position2 x1 y1) (Position2 x2 y2) = Position2 (x1 - x2) (y1 - y2)

pointApplyRelative ::
     Position2 -> (Position2 -> Position2) -> Position2 -> Position2
pointApplyRelative origin fn p = fn (p `pointMinus` origin) `pointPlus` origin

pointRotateLeft :: Position2 -> Position2
pointRotateLeft (Position2 x y) = Position2 y (-x)

pointRotateRight :: Position2 -> Position2
pointRotateRight (Position2 x y) = Position2 (-y) x

manhattanDistance :: Position2 -> Position2 -> Int
manhattanDistance (Position2 x1 y1) (Position2 x2 y2) =
  abs (x2 - x1) + abs (y2 - y1)

hammingDistance :: Position2 -> Position2 -> Int
hammingDistance (Position2 x1 y1) (Position2 x2 y2) =
  max (abs $ x2 - x1) (abs $ y2 - y1)

type Grid2 a = Map Position2 a

boundsG :: Grid2 a -> (Position2, Position2)
boundsG = pointBounds . Map.keys

insideBounds :: (Position2, Position2) -> Position2 -> Bool
insideBounds (Position2 xmin ymin, Position2 xmax ymax) (Position2 x y) =
  inRange xmin xmax x && inRange ymin ymax y

pointBounds :: [Position2] -> (Position2, Position2)
pointBounds [] = error "No bounds for empty list"
pointBounds ps = (Position2 xmin ymin, Position2 xmax ymax)
  where
    xmin = minimum $ map pX ps
    ymin = minimum $ map pY ps
    xmax = maximum $ map pX ps
    ymax = maximum $ map pY ps

shrinkWithG :: Int -> (a -> a -> a) -> Grid2 a -> Grid2 a
shrinkWithG scale fn = mapFromListWith fn . map (first scalePoint) . mapToList
  where
    scalePoint (Position2 x y) = Position2 (x `div` scale) (y `div` scale)

class GridItem a where
  showInGrid :: a -> Char

instance GridItem Char where
  showInGrid = id

instance GridItem () where
  showInGrid = const '#'

instance GridItem Int where
  showInGrid i
    | i < 0 = '-'
    | i > 10 = '+'
    | otherwise = chr (ord '0' + i)

instance GridItem Direction4 where
  showInGrid E = '>'
  showInGrid S = 'v'
  showInGrid W = '<'
  showInGrid N = '^'

showInGridMaybe :: GridItem a => Maybe a -> Char
showInGridMaybe = maybe middleDot showInGrid

displayG :: GridItem a => Grid2 a -> Text
displayG = displayG' showInGrid

middleDot :: Char
middleDot = '·'

displayG' :: (a -> Char) -> Grid2 a -> Text
displayG' fn =
  Text.unlines . map (Text.pack . map (maybe middleDot fn)) . toMatrixG

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

adjacent4 :: Position2 -> [Position2]
adjacent4 p = [walk d p | d <- allDir4]

adjacent8 :: Position2 -> [Position2]
adjacent8 p = [walk d p | d <- allDir8]

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

fromMatrixG :: [[a]] -> Grid2 a
fromMatrixG = mapFromList . concat . zipWith makeLine [0 ..]
  where
    makeLine y = zipWith (makePoint y) [0 ..]
    makePoint y x v = (Position2 x y, v)

fromSetG :: Set Position2 -> Grid2 ()
fromSetG = Map.fromSet (const ())

toMatrixG :: Map Position2 a -> [[Maybe a]]
toMatrixG m =
  [[mapLookup (Position2 x y) m | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG m
