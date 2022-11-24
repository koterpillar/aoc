module Y2021.Day11 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import           AOC
import           Grid
import           Utils

type Grid = Grid2 (Int, Bool)

mkGrid :: Grid2 Int -> Grid
mkGrid = fmap (, False)

step :: Grid -> Grid
step = execState go

countFlashes :: Grid -> Int
countFlashes = length . Map.filter snd

goP :: (Position2 -> State Grid ()) -> State Grid ()
goP fn = do
  ps <- gets Map.keys
  traverse_ fn ps

modifyP :: ((Int, Bool) -> State Grid (Int, Bool)) -> Position2 -> State Grid ()
modifyP fn p = do
  r <- gets $ mapLookup p
  forM_ r $ \rr -> do
    rr' <- fn rr
    modify $ Map.insert p rr'

incrP :: Position2 -> State Grid ()
incrP p = do
  r <- gets $ mapLookup p
  forM_ r $ \(v, f) -> do
    let v' = v + 1
    let f' = v' > 9
    modify $ Map.insert p (v', f')
    when (f' /= f) $ traverse_ (incrP . flip walk p) allDir8

go :: State Grid ()
go = do
  goP $ modifyP $ pure . \(v, _) -> (v, False)
  goP incrP
  goP $
    modifyP $
    pure . \case
      (_, True)  -> (0, True)
      (v, False) -> (v, False)

part1 :: Grid -> Int
part1 = go 0 100
  where
    go r 0 _ = r
    go r n grid =
      let grid' = step grid
       in go (r + countFlashes grid') (n - 1) grid'

part2 :: Grid -> Int
part2 = go 0
  where
    go n grid
      | countFlashes grid == length grid = n
      | otherwise = go (n + 1) (step grid)

tasks =
  Tasks
    2021
    11
    (CodeBlock 0)
    (mkGrid <$> digitGridP)
    [Task part1 1656, Task part2 195]
