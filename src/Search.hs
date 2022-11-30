module Search
  ( decideMapping
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Path
import           Utils

data Search k v =
  Search
    { sFound    :: Map k v
    , sPossible :: Map k (Set v)
    }
  deriving (Eq, Ord, Show)

instance (Hashable k, Hashable v) => Hashable (Search k v) where
  hashWithSalt x (Search a b) = hashWithSalt x (a, b)

pick :: Ord k => Map k (Set v) -> ((k, Set v), Map k (Set v))
pick m = ((k, vs), Map.delete k m)
  where
    (k, vs) = minimumBy (compare `on` (Set.size . snd)) $ Map.toList m

decideMapping ::
     (Ord k, Hashable k, Ord v, Hashable v) => Map k (Set v) -> Map k v
decideMapping n =
  sFound $
  lastE "empty route found" $
  fromJustE "no route found" $ dfs moves (length . sPossible) start
  where
    start = Search mempty n

moves ::
     (Hashable k, Hashable v, Ord k, Ord v)
  => Search k v
  -> HashSet (Search k v)
moves Search {..}
  | null sPossible = error "moves: already assigned everything"
  | otherwise =
    hashSetFromList $ do
      let ((r, poss), rp1) = pick sPossible
      i <- Set.toList poss
      let rp2 = Map.map (Set.delete i) rp1
      guard $ not $ any Set.null rp2
      pure $ Search (Map.insert r i sFound) rp2
