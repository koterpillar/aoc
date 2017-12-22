{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens

import Data.List.Utils

import Data.Either

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Text.Parsec
import Text.Parsec.String

import Utils

data CellState
  = Clean
  | Infected
  deriving (Ord, Eq)

instance Show CellState where
  show Clean = "."
  show Infected = "#"

toggle :: CellState -> CellState
toggle Clean = Infected
toggle Infected = Clean

type Grid = Map Position2 CellState

readGrid :: IO Grid
readGrid = parseGrid <$> readLines

parseGrid :: [String] -> Grid
parseGrid = Map.fromList . concat . enumerate2 . map (map chr2cs)
  where
    chr2cs '.' = Clean
    chr2cs '#' = Infected

gBounds :: Grid -> (Position2, Position2)
gBounds g = (Position2 xmin ymin, Position2 xmax ymax)
  where
    xmin = minimum xs
    xmax = maximum xs
    ymin = minimum ys
    ymax = maximum ys
    xs = map pX ps
    ys = map pY ps
    ps = Map.keys g

data World = World
  { _wGrid :: !Grid
  , _wVirusPos :: !Position2
  , _wVirusDir :: !Direction4
  } deriving (Ord, Eq)

makeLenses ''World

instance Show World where
  show w = unlines $ map showLine [ymin - 2 .. ymax + 2]
    where
      (Position2 xmin ymin, Position2 xmax ymax) = gBounds (w ^. wGrid)
      showLine y = concatMap (showPos y) [xmin - 2 .. xmax + 2]
      showPos y x
        | Position2 x y == w ^. wVirusPos = "[" ++ showDot y x ++ "]"
        | otherwise = " " ++ showDot y x ++ " "
      showDot y x = show (w ^. wAt (Position2 x y))

readWorld :: IO World
readWorld = wFromGrid <$> readGrid

parseWorld :: [String] -> World
parseWorld = wFromGrid . parseGrid

wFromGrid :: Grid -> World
wFromGrid g = World {..}
  where
    _wGrid = g
    _wVirusDir = N
    _wVirusPos = Position2 (xmin `mid` xmax) (ymin `mid` ymax)
    a `mid` b = (a + b) `div` 2
    (Position2 xmin ymin, Position2 xmax ymax) = gBounds g

wAt :: Position2 -> Lens' World CellState
wAt p = wGrid . at p . non Clean

step :: World -> World
step w = w & wAt curPos %~ toggle & wVirusPos .~ newPos & wVirusDir .~ newDir
  where
    curPos = w ^. wVirusPos
    curState = w ^. wAt curPos
    curDir = w ^. wVirusDir
    newDir =
      case curState of
        Clean -> turnLeft curDir
        Infected -> turnRight curDir
    newPos = walk newDir curPos

example :: World
example = parseWorld ["..#", "#..", "..."]
