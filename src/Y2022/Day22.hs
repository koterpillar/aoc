{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day22 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data GI0
  = Wall0
  | Open0
  | Blank0
  deriving (Show, Eq, Ord, Bounded, Enum)

data GI
  = Wall
  | Open Position2
  deriving (Show, Eq, Ord)

instance GridItem GI where
  showInGrid Wall    = '#'
  showInGrid Open {} = 'o'

itemP :: Parser Char GI0
itemP = choiceEBP "#. "

type Grid = Grid2 GI

mkgrid :: [[GI0]] -> Grid
mkgrid =
  Map.fromList . map wp . Map.toList . Map.filter (/= Blank0) . fromMatrixG
  where
    wp (p, Wall0) = (p, Wall)
    wp (p, Open0) = (p, Open p)

type Input = (Grid, [Instruction])

data Instruction
  = Forward Int
  | RotateLeft
  | RotateRight
  deriving (Ord, Eq, Show)

instructionsP :: Parser Text [Instruction]
instructionsP =
  pureP (Text.groupBy (\a b -> isDigit a == isDigit b)) &**
  (Forward <$> integerP) &|
  (RotateLeft <$ requireP "L") &|
  (RotateRight <$ requireP "R")

parser :: Parser Text Input
parser =
  lineGroupsP &*
  (fmap mkgrid (traverseP $ charactersP &** itemP) &+ (singleP &* instructionsP))

data You =
  You
    { _yPosition  :: Position2
    , _yDirection :: Direction4
    }
  deriving (Eq, Show)

makeLenses ''You

move :: Grid -> Instruction -> You -> You
move g (Forward i) =
  \y -> traceShowId $ y & yPosition %~ iterateNL i (step g (y ^. yDirection))
move _ RotateLeft = yDirection %~ turnLeft
move _ RotateRight = yDirection %~ turnRight

step :: Grid -> Direction4 -> Position2 -> Position2
step g d p =
  if Map.lookup p1 g == Just Wall
    then p
    else p1
  where
    p1 = wrapWalk g d p
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

wrapWalk1 :: Grid -> Direction4 -> Position2 -> Position2
wrapWalk1 g d p = Position2 (w xmin xmax x) (w ymin ymax y)
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    (Position2 x y) = walk d p
    w a b c
      | c > b = c - b + a - 1
      | c < a = c + b - a + 1
      | otherwise = c

wrapWalk :: Grid -> Direction4 -> Position2 -> Position2
wrapWalk g d p =
  if Map.member p' g
    then p'
    else wrapWalk g d p'
  where
    p' = wrapWalk1 g d p

start :: Grid -> You
start g = You p E
  where
    p = wrapWalk g E topLeft
    (topLeft, _) = boundsG g

facingScore :: Direction4 -> Int
facingScore E = 0
facingScore S = 1
facingScore W = 2
facingScore N = 3

score :: Grid -> You -> Int
score g (You ep ed) = 1000 * (y + 1) + 4 * (x + 1) + facingScore ed
  where
    Open (Position2 x y) = mapLookupE "score" ep g

part1 :: (Grid, [Instruction]) -> Int
part1 (g, is) = score g $ foldl' (flip $ move g) p is
  where
    p = traceShowId $ start $ ttraceF displayG g

subgrid :: Int -> Position2 -> Grid2 a -> Grid2 a
subgrid sz origin =
  Map.filterWithKey (\(Position2 x y) _ -> inRange 0 sz x && inRange 0 sz y) .
  Map.mapKeys (`pointMinus` origin)

rotgridR :: Grid2 a -> Grid2 a
rotgridR g = Map.mapKeys (\(Position2 x y) -> Position2 (d - y) x) g
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g
    d = xmax - xmin

rotgridUD :: Grid2 a -> Grid2 a
rotgridUD = rotgridR . rotgridR

rotgridL :: Grid2 a -> Grid2 a
rotgridL = rotgridR . rotgridR . rotgridR

chunksize :: Grid2 a -> Int
chunksize = round . sqrt . fromIntegral . (`div` 6) . length

part2 :: (Grid, [Instruction]) -> Int
part2 = error . show

tasks = Tasks 2022 22 (CodeBlock 0) parser [Task part1 6032, Task part2 5031]
