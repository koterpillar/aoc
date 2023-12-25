module Graph where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import           Utils

type Graph v = Map v (Set v)

mapGraph :: Ord u => (v -> u) -> Graph v -> Graph u
mapGraph f = mapFromListS . map (bimap f $ Set.map f) . Map.toList

vicinity :: Ord v => Set v -> Graph v -> Set v
vicinity vs graph = vs <> mconcat (mapMaybe (`mapLookup` graph) $ toList vs)

reverseGraph :: Ord v => Graph v -> Graph v
reverseGraph graph = Map.fromListWith mappend neighbors
  where
    neighbors = do
      (v, vs) <- Map.toList graph
      v' <- toList vs
      pure (v', set1 v)

bidirectional :: Ord v => Graph v -> Graph v
bidirectional graph =
  mapFromListS $ Map.toList graph <> Map.toList (reverseGraph graph)

reachableFrom :: Ord v => v -> Graph v -> Set v
reachableFrom v graph = iterateSettleL (`vicinity` graph) (set1 v)

vertices :: Graph v -> Set v
vertices = Map.keysSet

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

dot :: (v -> Text) -> Graph v -> Text
dot fmt =
  header . map (uncurry edge) . filter (not . Set.null . snd) . Map.toList
  where
    header ls = "digraph {" <> Text.intercalate ";" ls <> "}"
    edge a bs = fmt a <> "->" <> Text.intercalate "," (map fmt $ Set.toList bs)
