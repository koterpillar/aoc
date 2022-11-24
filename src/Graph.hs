module Graph where

import qualified Data.Map.Strict as SMap
import qualified Data.Set        as Set

import           Utils

type Graph v = Map v (Set v)

vicinity :: Ord v => Set v -> Graph v -> Set v
vicinity vs graph = vs <> mconcat (mapMaybe (`mapLookup` graph) $ toList vs)

reverseGraph :: Ord v => Graph v -> Graph v
reverseGraph graph = SMap.fromListWith mappend neighbors
  where
    neighbors = do
      (v, vs) <- SMap.toList graph
      v' <- toList vs
      pure (v', Set.singleton v)

reachableFrom :: Ord v => v -> Graph v -> Set v
reachableFrom v graph = iterateSettle (`vicinity` graph) (Set.singleton v)

vertices :: Graph v -> Set v
vertices = SMap.keysSet

unreachableFrom :: Ord v => v -> Graph v -> Set v
unreachableFrom v graph = vertices graph `Set.difference` reachableFrom v graph

connectedComponents :: Ord v => Graph v -> [Set v]
connectedComponents graph = go (vertices graph)
  where
    go candidates
      | null candidates = []
      | otherwise =
        let candidate = head $ toList candidates
            component = reachableFrom candidate graph
         in component : go (candidates `Set.difference` component)
