{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day18 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map

import           AOC
import           Utils

type Cube = [Int]

parser :: Parser Text [Cube]
parser = linesP &** tsplitP "," &** integerP

adjanced :: Cube -> [Cube]
adjanced a = [a & ix i %~ f | i <- [0 .. length a - 1], f <- [succ, pred]]

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
    { _st1map :: Map Cube St
    , _st1min :: Cube
    , _st1max :: Cube
    }

makeLenses ''St1

stlookup :: Cube -> State St1 (Maybe St)
stlookup c = do
  St1 {..} <- get
  pure $
    if and (zipWith3 inRange _st1min _st1max c)
      then Map.lookup c _st1map
      else Just Outside

stinsert :: Cube -> St -> State St1 ()
stinsert c v = st1map %= Map.insert c v

stremove :: Cube -> State St1 ()
stremove c = st1map %= Map.delete c

stsetpending :: St -> State St1 ()
stsetpending t = st1map %= Map.map s
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

part2 input = countTrue $ evalState (traverse isOutside sides) St1 {..}
  where
    sides = freeSides input
    _st1map = Map.fromList $ map (, Lava) input
    (_st1min, _st1max) = bounds input

tasks = Tasks 2022 18 (CodeBlock 0) parser [Task part1 64, Task part2 58]
