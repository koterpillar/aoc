module Utils.Map where

import           Data.Foldable

import           Data.Map      (Map)
import qualified Data.Map      as Map

import           Data.Maybe

import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Utils.Base

mapLookupE :: (Ord k, Show k) => String -> k -> Map k a -> a
mapLookupE msg k m =
  fromMaybe (error $ "mapLookupE: " ++ msg ++ ": " ++ show k) $ Map.lookup k m

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = Map.fromList

mapFromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
mapFromListWith = Map.fromListWith

mapFromListS :: (Ord k, Semigroup a) => [(k, a)] -> Map k a
mapFromListS = Map.fromListWith (<>)

mapFromListSum :: (Ord k, Num a) => [(k, a)] -> Map k a
mapFromListSum = Map.fromListWith (+)

mapFromListCount :: (Ord k, Num a) => [k] -> Map k a
mapFromListCount = mapFromListSum . map (, 1)

mapSum :: (Ord k, Num a) => Map k a -> Map k a -> Map k a
mapSum = Map.unionWith (+)

mapToList :: Map k a -> [(k, a)]
mapToList = Map.toList

mapMember :: Ord k => k -> Map k a -> Bool
mapMember = Map.member

mapLookup :: Ord k => k -> Map k a -> Maybe a
mapLookup = Map.lookup

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Map.insert

map1 :: Ord k => k -> a -> Map k a
map1 = Map.singleton

mapElemsSet :: Ord a => Map k a -> Set a
mapElemsSet = Set.fromList . Map.elems

mapFindValue :: (a -> Bool) -> Map k a -> Maybe k
mapFindValue fn = findTuple fn . Map.toList

mapFindValueE :: String -> (a -> Bool) -> Map k a -> k
mapFindValueE msg fn = fromJustE msg . mapFindValue fn

mapFilterValues :: (a -> Bool) -> Map k a -> [k]
mapFilterValues fn = filterTuple fn . Map.toList

mapFilterMap :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapFilterMap = mapFilterMapWithKey . const

mapFilterMapWithKey :: Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
mapFilterMapWithKey fn = Map.fromList . mapMaybe go . Map.toList
  where
    go (k, v) = (k, ) <$> fn k v
