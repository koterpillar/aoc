module Graph where

import Data.List.Utils

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

type Graph v = Map v (Set v)

vicinity :: Ord v => Set v -> Graph v -> Set v
vicinity vs graph =
  vs <> mconcat (mapMaybe (`Map.lookup` graph) $ Set.toList vs)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x =
  let x' = f x
  in if x == x'
       then x
       else fixPoint f x'

reachableFrom :: Ord v => v -> Graph v -> Set v
reachableFrom v graph = fixPoint (`vicinity` graph) (Set.singleton v)

vertices :: Graph v -> Set v
vertices = Map.keysSet

unreachableFrom :: Ord v => v -> Graph v -> Set v
unreachableFrom v graph = vertices graph `Set.difference` reachableFrom v graph

connectedComponents :: Ord v => Graph v -> [Set v]
connectedComponents graph = go (vertices graph)
  where
    go candidates
      | Set.null candidates = []
      | otherwise =
        let candidate = head $ Set.toList candidates
            component = reachableFrom candidate graph
        in component : go (candidates `Set.difference` component)
