module Utils.Set where

import           Data.Maybe

import           Data.Set   (Set)
import qualified Data.Set   as Set

set1 :: a -> Set a
set1 = Set.singleton

setFromList :: Ord a => [a] -> Set a
setFromList = Set.fromList

setMember :: Ord a => a -> Set a -> Bool
setMember = Set.member

setInsert :: Ord a => a -> Set a -> Set a
setInsert = Set.insert

setMap :: Ord b => (a -> b) -> Set a -> Set b
setMap = Set.map

setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe fn = Set.fromList . mapMaybe fn . Set.toList

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference = Set.difference

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection = Set.intersection
