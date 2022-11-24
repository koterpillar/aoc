module Quantum
  ( Quantum(..)
  , Collapse(..)
  , all
  , filter
  , flatMap
  , fromList
  , fromListSingle
  , join
  , map
  , pure
  , singleton
  , size
  , toList
  , values
  ) where

import           Data.Map       (Map)
import qualified Data.Map       as Map

import           Data.Bifunctor (first)

import           Prelude        hiding (all, filter, map, pure)
import qualified Prelude

class Collapse a where
  cinit :: a
  cappend :: a -> a -> a
  collapse :: a -> a -> a

newtype Quantum collapse a =
  Quantum
    { getQuantum :: Map a collapse
    }
  deriving (Eq, Ord, Show)

size :: Quantum collapse a -> Int
size = length . getQuantum

toList :: Quantum collapse a -> [(a, collapse)]
toList = Map.toList . getQuantum

values :: Quantum collapse a -> [a]
values = Map.keys . getQuantum

fromList :: (Collapse collapse, Ord a) => [(a, collapse)] -> Quantum collapse a
fromList = Quantum . Map.fromListWith collapse

fromListSingle :: (Collapse collapse, Ord a) => [a] -> Quantum collapse a
fromListSingle = fromList . flip zip (repeat cinit)

singleton :: (Collapse collapse, Ord a) => a -> Quantum collapse a
singleton a = Quantum $ Map.singleton a cinit

pure :: (Collapse collapse, Ord a) => a -> Quantum collapse a
pure = singleton

map ::
     (Collapse collapse, Ord b)
  => (a -> b)
  -> Quantum collapse a
  -> Quantum collapse b
map f = fromList . Prelude.map (first f) . toList

join ::
     (Collapse collapse, Ord a)
  => Quantum collapse (Quantum collapse a)
  -> Quantum collapse a
join = fromList . concatMap j . toList
  where
    j (ca, c) = [(a, c `cappend` c') | (a, c') <- toList ca]

flatMap ::
     (Collapse collapse, Ord b)
  => Quantum collapse a
  -> (a -> Quantum collapse b)
  -> Quantum collapse b
flatMap a f = fromList $ concatMap f' $ toList a
  where
    f' (a, c) = [(b, c `cappend` c') | (b, c') <- toList $ f a]

filter :: (a -> Bool) -> Quantum collapse a -> Quantum collapse a
filter f = Quantum . Map.filterWithKey (\k _ -> f k) . getQuantum

all :: (a -> Bool) -> Quantum collapse a -> Bool
all f = Prelude.all (f . fst) . toList
