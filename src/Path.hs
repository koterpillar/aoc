{-# LANGUAGE DeriveFoldable #-}

module Path
  ( module Path
  , module Data.Graph.AStar
  , HashSet
  , Hashable(..)
  ) where

import           Data.Graph.AStar

import           Data.Hashable    (Hashable (..))

import           Data.HashSet     (HashSet)
import qualified Data.HashSet     as HashSet

import qualified Data.Map         as Map

import           Utils

hashSetFromList :: (Eq a, Hashable a) => [a] -> HashSet a
hashSetFromList = HashSet.fromList

data Tree edge node =
  Tree
    { treeNode     :: node
    , treeBranches :: [(edge, Tree edge node)]
    }
  deriving (Ord, Eq, Show, Foldable)

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
