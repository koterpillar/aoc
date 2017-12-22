{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace

import Utils

data CellState
  = Clean
  | Infected
  | Weakened
  | Flagged
  deriving (Ord, Eq)

instance Show CellState where
  show Clean = "."
  show Infected = "#"
  show Weakened = "W"
  show Flagged = "F"

toggle :: CellState -> CellState
toggle Clean = Weakened
toggle Weakened = Infected
toggle Infected = Flagged
toggle Flagged = Clean

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
  , _wTurnsInfected :: !Int
  , _wSteps :: !Int
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
    _wTurnsInfected = 0
    _wSteps = 0

wAt :: Position2 -> Lens' World CellState
wAt p = wGrid . at p . non Clean

step :: World -> World
step (!w) =
  w & wAt curPos .~ newState & wVirusPos .~ newPos & wVirusDir .~ newDir &
  wTurnsInfected +~
  infected &
  wSteps +~
  progress 10000 (w ^. wSteps) 1
  where
    curPos = w ^. wVirusPos
    curState = w ^. wAt curPos
    curDir = w ^. wVirusDir
    newDir =
      case curState of
        Clean -> turnLeft curDir
        Infected -> turnRight curDir
        Weakened -> curDir
        Flagged -> reverse4 curDir
    newPos = walk newDir curPos
    newState = toggle curState
    infected =
      if newState == Infected
        then 1
        else 0

example :: World
example = parseWorld ["..#", "#..", "..."]
