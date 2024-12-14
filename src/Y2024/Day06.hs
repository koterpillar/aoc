module Y2024.Day06
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Grid
import           Utils

data Itm
  = Obstacle
  | Guard Direction4
  deriving (Eq, Ord, Show)

instance Bounded Itm where
  minBound = Obstacle
  maxBound = Guard maxBound

instance Enum Itm where
  toEnum 0 = Obstacle
  toEnum n = Guard (toEnum (n - 1))
  fromEnum Obstacle  = 0
  fromEnum (Guard d) = fromEnum d + 1

instance GridItem Itm where
  showInGrid Obstacle  = '#'
  showInGrid (Guard d) = showInGrid d

type Input = Grid2 Itm

type GuardPos = (Position2, Direction4)

type Bounds = (Position2, Position2)

type Input2 = (GuardPos, Grid2 (), Bounds)

convertInput :: Input -> Input2
convertInput g0 = (pd, g, boundsG g)
  where
    g = void $ Map.filter (== Obstacle) g0
    pd =
      fromJustE "No guard"
        $ listToMaybe [(p, d) | (p, Guard d) <- Map.toList g0]

parser :: Parser Text Input2
parser = convertInput <$> charGridP

data Outcome
  = Outside [GuardPos]
  | Loop
  | Pending
  deriving (Ord, Eq, Show)

nextOutcome :: GuardPos -> Outcome -> Outcome
nextOutcome p (Outside ps) = Outside $ p : ps
nextOutcome _ Pending      = Loop
nextOutcome _ Loop         = Loop

cachedState :: Ord k => k -> State (Map k v) v -> State (Map k v) v
cachedState k f =
  gets (Map.lookup k) >>= \case
    Just v -> pure v
    Nothing -> f >>= \v -> modify (Map.insert k v) $> v

cachedOutcome ::
     Ord k
  => k
  -> State (Map k Outcome) Outcome
  -> State (Map k Outcome) Outcome
cachedOutcome k f = cachedState k $ modify (Map.insert k Pending) >> f

walk1 :: Grid2 () -> Bounds -> GuardPos -> Outcome
walk1 grid bounds pd@(p, d) = evalState (walk1s grid bounds pd) Map.empty

walk1s :: Grid2 () -> Bounds -> GuardPos -> State (Map GuardPos Outcome) Outcome
walk1s grid bounds pd@(p, d)
  | not $ insideBounds bounds p = pure $ Outside []
  | otherwise =
    cachedOutcome pd $ do
      let pS = walk d p
      let p' =
            if Map.member pS grid
              then (p, turnRight d)
              else (pS, d)
      r' <- walk1s grid bounds p'
      pure $ nextOutcome pd r'

showWalk :: GridItem i => Grid2 () -> [(Position2, i)] -> Grid2 Char
showWalk g path =
  flip execState (fmap (const '#') g) $ do
    for_ path $ \(p, d) -> modify $ Map.insert p $ showInGrid d

part1 :: Input2 -> Int
part1 (start, grid, bounds) =
  case walk1 grid bounds start of
    (Outside ps) ->
      length $ nubOrd $ map fst $ ttraceF (displayG . showWalk grid) ps
    Loop -> error "unexpected loop"

part2 :: Input2 -> Int
part2 (start, grid, bounds) = countIf makesLoop allObstacles
  where
    (Position2 xmin ymin, Position2 xmax ymax) = bounds
    makesLoop p = walk1 (Map.insert p () grid) bounds start == Loop
    allObstacles = do
      x <- [xmin .. xmax]
      y <- [ymin .. ymax]
      let p = Position2 x y
      guard $ not $ Map.member p grid
      pure p

tasks =
  Tasks
    2024
    6
    (CodeBlock 0)
    parser
    [task part1 41 & taskPart 1, task part2 6 & taskPart 2]
