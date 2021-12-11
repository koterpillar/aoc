import           Control.Monad.State

import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Data.Set            (Set)
import qualified Data.Set            as Set

import           AOC
import           Grid
import           Utils

type Grid = Map Position2 (Int, Bool)

mkGrid :: [[Int]] -> Grid
mkGrid = Map.map (, False) . enumerate2

step :: Grid -> Grid
step = execState go

countFlashes :: Grid -> Int
countFlashes = Map.size . Map.filter snd

goP :: (Position2 -> State Grid ()) -> State Grid ()
goP fn = do
  ps <- gets Map.keys
  traverse fn ps
  pure ()

modifyP :: ((Int, Bool) -> State Grid (Int, Bool)) -> Position2 -> State Grid ()
modifyP fn p = do
  r <- gets $ Map.lookup p
  forM_ r $ \rr -> do
    rr' <- fn rr
    modify $ Map.insert p rr'

incrP :: Position2 -> State Grid ()
incrP p = do
  r <- gets $ Map.lookup p
  forM_ r $ \(v, f) -> do
    let v' = v + 1
    let f' = v' > 9
    modify $ Map.insert p (v', f')
    when (f' /= f) $ do
      traverse incrP $ map (flip walk p) allDir8
      pure ()

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
      | countFlashes grid == Map.size grid = n
      | otherwise = go (n + 1) (step grid)

main = do
  processEI 11 (mkGrid . parseLines digitsP) part1 1656
  processEI 11 (mkGrid . parseLines digitsP) part2 195
