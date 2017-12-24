{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}

module Path where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Utils

data Tree edge node = Tree
  { treeNode :: node
  , treeBranches :: [(edge, Tree edge node)]
  } deriving (Ord, Eq, Show, Foldable)

moveTree ::
     (pos -> [move]) -> (move -> pos -> Maybe pos) -> pos -> Tree move pos
moveTree generate apply = mtc
  where
    mtc start = Tree start children
      where
        childNodes =
          catMaybes [(move, ) <$> apply move start | move <- generate start]
        children = map go childNodes
        go (move, childNode) = (move, mtc childNode)

uniqBy :: Ord b => (a -> b) -> [a] -> [a]
uniqBy key = M.elems . M.fromList . map (\a -> (key a, a))

levels :: Ord key => (pos -> key) -> Tree move pos -> [[pos]]
levels posKey start =
  map (map treeNode . fst) $
  iterateWhile (not . null . fst) (uncurry go) ([start], M.empty)
  where
    mkKey p = (posKey p, p)
    go roots seen = (nextRoots, seen')
      where
        nextRoots =
          filter (not . flip M.member seen . posKey . treeNode) $
          uniqBy (posKey . treeNode) (concatMap (map snd . treeBranches) roots)
        seen' = M.union seen $ M.fromList $ map (mkKey . treeNode) nextRoots

shortestPath :: (pos -> Bool) -> [[pos]] -> [[pos]]
shortestPath success (thisLevel:rest) =
  let done = filter success thisLevel
  in if null done
       then [] : shortestPath success rest
       else [done]
