module Graph where

import qualified Data.Map.Strict as SMap

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
      pure (v', set1 v)

reachableFrom :: Ord v => v -> Graph v -> Set v
reachableFrom v graph = iterateSettleL (`vicinity` graph) (set1 v)

vertices :: Graph v -> Set v
vertices = SMap.keysSet

unreachableFrom :: Ord v => v -> Graph v -> Set v
unreachableFrom v graph = vertices graph `setDifference` reachableFrom v graph

connectedComponents :: Ord v => Graph v -> [Set v]
connectedComponents graph = go (vertices graph)
  where
    go candidates
      | null candidates = []
      | otherwise =
        let candidate = head $ toList candidates
            component = reachableFrom candidate graph
         in component : go (candidates `setDifference` component)
