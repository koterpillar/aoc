{-# LANGUAGE Strict #-}

module Y2022.Day18 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map

import           AOC
import           Utils

type Cube = [Int]

parser :: Parser Text [Cube]
parser = linesP &** tsplitP "," &** integerP

adjanced :: Cube -> [Cube]
adjanced a =
  [sset i (f $ a !! i) a | i <- [0 .. length a - 1], f <- [succ, pred]]

freeSides :: [Cube] -> [Cube]
freeSides cubes = filter (not . (`elem` cubes)) $ concatMap adjanced cubes

part1 = length . freeSides

data St
  = Lava
  | Pending
  | Inside
  | Outside
  deriving (Eq, Show)

data St1 =
  St1
    { st1m   :: Map Cube St
    , st1min :: Cube
    , st1max :: Cube
    }

stlookup :: Cube -> State St1 (Maybe St)
stlookup c = do
  St1 {..} <- get
  pure $
    if and (zipWith3 inRange st1min st1max c)
      then Map.lookup c st1m
      else Just Outside

stinsert :: Cube -> St -> State St1 ()
stinsert c v = modify $ \st1 -> st1 {st1m = Map.insert c v $ st1m st1}

stremove :: Cube -> State St1 ()
stremove c = modify $ \st1 -> st1 {st1m = Map.delete c $ st1m st1}

stsetpending :: St -> State St1 ()
stsetpending t = modify $ \st1 -> st1 {st1m = Map.map s $ st1m st1}
  where
    s Pending = t
    s x       = x

isPending :: Cube -> State St1 Bool
isPending c = (== Just Pending) <$> stlookup c

canGo :: Cube -> State St1 Bool
canGo c = do
  v <- stlookup c
  pure $ v /= Just Pending && v /= Just Lava

isOutside :: Cube -> State St1 Bool
isOutside c = do
  res <- fromMaybe False <$> isOutside' c
  stsetpending $
    if res
      then Outside
      else Inside
  pure res

isOutside' :: Cube -> State St1 (Maybe Bool)
isOutside' c =
  stlookup c >>= \case
    Just Lava -> error $ "trying to go into another lava " <> show c
    Just Pending -> pure Nothing
    Just Inside -> pure $ Just False
    Just Outside -> pure $ Just True
    Nothing -> do
      stinsert c Pending
      let a0 = adjanced c
      a1 <- filterM canGo a0
      res <- condense <$> traverse isOutside' a1
      for_ res $ \case
        True  -> stinsert c Outside
        False -> stinsert c Inside
      pure res

condense :: [Maybe Bool] -> Maybe Bool
condense []             = Nothing
condense (Just True:_)  = Just True
condense (Just False:_) = Just False
condense (Nothing:r)    = condense r

bounds :: [Cube] -> (Cube, Cube)
bounds cubes = (map minimum tc, map maximum tc)
  where
    tc = transpose cubes

walls :: Cube -> Cube -> [Cube]
walls [] [] = [[]]
walls (min:mins) (max:maxs) = do
  x <- [min - 1, max + 1]
  xs <- walls mins maxs
  pure $ x : xs

part2 input = countTrue $ evalState (traverse isOutside sides) initSt
  where
    initSt =
      St1
        {st1m = Map.fromList $ map (, Lava) input, st1min = smin, st1max = smax}
    sides = freeSides input
    (smin, smax) = bounds input

tasks = Tasks 2022 18 (CodeBlock 0) parser [Task part1 64, Task part2 58]
