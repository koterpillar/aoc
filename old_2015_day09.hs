import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

type City = String

type Routes = Map City (Map City Int)

parseRoutes :: IO Routes
parseRoutes = go <$> readLines
  where
    go = foldr addCity Map.empty
    addCity :: String -> Routes -> Routes
    addCity str =
      case words str of
        [c1, "to", c2, "=", dist] ->
          add' c1 c2 (read dist) . add' c2 c1 (read dist)
    add' c1 c2 dist =
      Map.alter (Just . Map.insert c2 dist . fromMaybe Map.empty) c1

paths :: Routes -> [Int]
paths r = do
  start <- Map.keys r
  go start (Set.delete start $ Map.keysSet r)
  where
    go start targets
      | Set.null targets = pure 0
      | otherwise = do
        (next, dist) <-
          Map.toList $
          Map.filterWithKey (\c d -> c `Set.member` targets) $
          fromMaybe Map.empty $ Map.lookup start r
        fromNext <- go next (Set.delete next targets)
        pure $ dist + fromNext
