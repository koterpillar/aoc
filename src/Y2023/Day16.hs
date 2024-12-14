module Y2023.Day16
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Grid
import           Utils

data Item
  = MirrorF
  | MirrorB
  | SplitterH
  | SplitterV
  deriving (Eq, Ord, Show, Enum, Bounded)

applyItem :: Maybe Item -> Direction4 -> [Direction4]
applyItem Nothing dir        = [dir]
applyItem (Just MirrorF) E   = [N]
applyItem (Just MirrorF) W   = [S]
applyItem (Just MirrorF) N   = [E]
applyItem (Just MirrorF) S   = [W]
applyItem (Just MirrorB) E   = [S]
applyItem (Just MirrorB) W   = [N]
applyItem (Just MirrorB) N   = [W]
applyItem (Just MirrorB) S   = [E]
applyItem (Just SplitterH) E = [E]
applyItem (Just SplitterH) W = [W]
applyItem (Just SplitterH) N = [E, W]
applyItem (Just SplitterH) S = [E, W]
applyItem (Just SplitterV) E = [N, S]
applyItem (Just SplitterV) W = [N, S]
applyItem (Just SplitterV) N = [N]
applyItem (Just SplitterV) S = [S]

instance GridItem Item where
  showInGrid MirrorF   = '/'
  showInGrid MirrorB   = '\\'
  showInGrid SplitterH = '-'
  showInGrid SplitterV = '|'

type Grid = Grid2 Item

type Beams = Grid2 [Direction4]

beamExists :: Position2 -> Direction4 -> State Beams Bool
beamExists pos dir = do
  existing <- gets (fromMaybe [] . Map.lookup pos)
  pure $ dir `elem` existing

parser :: Parser Text Grid
parser = charGridP

energized :: Grid -> Position2 -> Direction4 -> Int
energized g p d = length beams
  where
    beams = execState (runBeam g p d) Map.empty

part1 g = energized g (Position2 0 0) E

part2 g = maximum [energized g p d | (p, d) <- n]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    n = [(Position2 x ymin, S) | x <- [xmin .. xmax]]
    s = [(Position2 x ymax, N) | x <- [xmin .. xmax]]
    w = [(Position2 xmin y, E) | y <- [ymin .. ymax]]
    e = [(Position2 xmax y, W) | y <- [ymin .. ymax]]

runBeam :: Grid -> Position2 -> Direction4 -> State Beams ()
runBeam g p d = do
  exists <- beamExists p d
  if not (insideBounds (boundsG g) p) || exists
    then pure ()
    else do
      modify $ Map.alter (Just . (d :) . fromMaybe []) p
      let item = Map.lookup p g
      let ds' = applyItem item d
      traverse_ (\d' -> runBeam g (walk d' p) d') ds'

tasks = Tasks 2023 16 (CodeBlock 0) parser [Task part1 46, Task part2 51]
