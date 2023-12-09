module Cycle
  ( Cycle(..)
  , cycleFind
  , cycleMap
  , cycleGenerate
  ) where

import qualified Data.Map as Map

import           Utils

data Cycle st =
  Cycle
    { cycleStart :: [st]
    , cycleLoop  :: [st]
    }
  deriving (Show, Eq, Ord)

cycleFind :: (Ord st, Show st) => [st] -> Cycle st
cycleFind = go [] 0 Map.empty
  where
    go els n indices (x:xs) =
      case Map.lookup x indices of
        Just i ->
          let (start, loop) = splitAt i $ reverse els
           in Cycle start loop
        Nothing -> go (x : els) (succ n) (Map.insert x n indices) xs

cycleMap :: (Ord b, Show b) => (a -> b) -> Cycle a -> Cycle b
cycleMap f = cycleFind . map f . cycleGenerate

cycleGenerate :: Cycle st -> [st]
cycleGenerate (Cycle start loop) = start ++ cycle loop
