module Graph where

import qualified Data.Map.Strict as SMap
import qualified Data.Set        as Set

import           Utils

type Graph v = SMap.Map v (Set v)

vicinity :: Ord v => Set v -> Graph v -> Set v
vicinity vs graph =
  vs <> mconcat (mapMaybe (`SMap.lookup` graph) $ Set.toList vs)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x =
  let x' = f x
   in if x == x'
        then x
        else fixPoint f x'

reachableFrom :: Ord v => v -> Graph v -> Set v
reachableFrom v graph = fixPoint (`vicinity` graph) (Set.singleton v)

vertices :: Graph v -> Set v
vertices = SMap.keysSet

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
