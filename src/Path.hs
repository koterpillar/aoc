{-# LANGUAGE DeriveFoldable #-}

module Path
  ( module Path
  , HashSet
  , Hashable(..)
  ) where

import qualified Data.Graph.AStar as AStar

import           Data.Hashable    (Hashable (..))

import           Data.HashSet     (HashSet)
import qualified Data.HashSet     as HashSet

import           Utils

hashSetFromList :: (Eq a, Hashable a) => [a] -> HashSet a
hashSetFromList = HashSet.fromList

data Tree edge node = Tree
  { treeNode     :: node
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

aStar ::
     (Hashable a, Ord a, Ord c, Num c)
  => (a -> [a]) -- ^ possible moves
  -> (a -> a -> c) -- ^ cost of the move
  -> (a -> c) -- ^ estimate distance to goal, MUST NOT overestimate
  -> (a -> Bool) -- ^ is this the goal?
  -> a -- ^ start
  -> Maybe [a] -- ^ path to goal
aStar moves = AStar.aStar (hashSetFromList . moves)

aStarDepth ::
     (Hashable a, Ord a, Ord c, Num c)
  => (a -> [a]) -- ^ possible moves
  -> (a -> c) -- ^ estimate distance to goal, MUST NOT overestimate
  -> (a -> Bool) -- ^ is this the goal?
  -> a -- ^ start
  -> Maybe [a] -- ^ path to goal
aStarDepth moves = aStar moves (const $ const 1)

aStarDepthGoal ::
     (Hashable a, Ord a, Ord c, Num c)
  => (a -> [a]) -- ^ possible moves
  -> (a -> c) -- ^ estimate distance to goal, MUST NOT overestimate, MUST be zero at the goal
  -> a -- ^ start
  -> Maybe [a] -- ^ path to goal
aStarDepthGoal moves distanceToGoal = aStarDepth moves distanceToGoal isGoal
  where
    isGoal = (== 0) . distanceToGoal
