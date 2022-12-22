{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day22 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data GI
  = Wall
  | Open
  | Blank
  deriving (Show, Eq, Ord, Bounded, Enum)

instance GridItem GI where
  showInGrid Wall  = '#'
  showInGrid Open  = 'o'
  showInGrid Blank = ' '

itemP :: Parser Char GI
itemP = choiceEBP "#. "

type Grid = Grid2 GI

data Instruction
  = Forward Int
  | RotateLeft
  | RotateRight
  deriving (Ord, Eq, Show)

data You =
  You
    { _yPosition  :: Position2
    , _yDirection :: Direction4
    }
  deriving (Eq, Show)

makeLenses ''You

move :: Grid -> Instruction -> You -> You
move g (Forward i) =
  \y -> y & yPosition %~ iterateNL i (step g (y ^. yDirection))
move _ RotateLeft = yDirection %~ turnLeft
move _ RotateRight = yDirection %~ turnRight

glookup :: Position2 -> Grid -> GI
glookup k = fromMaybe Blank . Map.lookup k

step :: Grid -> Direction4 -> Position2 -> Position2
step g d p =
  if glookup p1 g == Wall
    then p
    else p1
  where
    p1 = wrapWalk g d p
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

type Input = (Grid, [Instruction])

instructionsP :: Parser Text [Instruction]
instructionsP =
  pureP (Text.groupBy (\a b -> isDigit a == isDigit b)) &**
  (Forward <$> integerP) &|
  (RotateLeft <$ requireP "L") &|
  (RotateRight <$ requireP "R")

mkgrid :: [[GI]] -> Grid
mkgrid = Map.filter (/= Blank) . fromMatrixG

parser :: Parser Text Input
parser =
  lineGroupsP &*
  (fmap mkgrid (traverseP $ charactersP &** itemP) &+ (singleP &* instructionsP))

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
  if glookup p' g == Blank
    then wrapWalk g d p'
    else p'
  where
    p' = wrapWalk1 g d p

start :: Grid -> You
start g = You p E
  where
    p = wrapWalk g E topLeft
    (topLeft, _) = boundsG g

part1 (g, is) = 1000 * (pX ep + 1) + 4 * (pY ep + 1) + fromEnum ed
  where
    p = start $ ttraceF displayG g
    (You ep ed) = foldl' (flip $ move g) p is

tasks = Tasks 2022 22 (CodeBlock 0) parser [Task part1 6032]
