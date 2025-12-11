module Y2020.Day15
  ( tasks
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector

import           AOC
import           Utils

data State s = State
  { sTurns    :: STRef s Int
  , sRecent   :: MVector s Int
  , sLastSaid :: STRef s Int
  }

say :: State s -> Int -> ST s ()
say State {..} n = do
  ls <- readSTRef sLastSaid
  t <- readSTRef sTurns
  MVector.write sRecent ls t
  modifySTRef sTurns succ
  writeSTRef sLastSaid n

nextNumber :: State s -> ST s Int
nextNumber State {..} = do
  ls <- readSTRef sLastSaid
  t <- readSTRef sTurns
  r <- MVector.read sRecent ls
  pure
    $ if r == 0
        then 0
        else t - r

firstState :: Int -> Int -> ST s (State s)
firstState max n = do
  sRecent <- MVector.new max
  sTurns <- newSTRef 1
  sLastSaid <- newSTRef n
  pure $ State {..}

turn :: State s -> ST s ()
turn s = nextNumber s >>= say s

part1 n input =
  runST $ do
    let (i1:irest) = input
    s <- firstState n i1
    traverse_ (say s) irest
    let turns = n - length input
    replicateM_ turns (turn s)
    readSTRef (sLastSaid s)

tasks =
  Tasks
    (AOC 2020 15)
    (Inline "0,3,6")
    (tsplitP "," &** integerP)
    [Task (part1 2020) 436, Task (part1 30000000) 175594]
