module Y2023.Day05 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Range =
  Range
    { rDst :: Int
    , rSrc :: Int
    , rLen :: Int
    }
  deriving (Ord, Eq, Show)

rApply :: Range -> Int -> Maybe Int
rApply (Range dst src len) idx
  | idx >= src && idx < src + len = Just $ dst + idx - src
  | otherwise = Nothing

newtype Transform =
  Transform
    { tRanges :: [Range]
    }
  deriving (Ord, Eq, Show)

tApply :: Transform -> Int -> Int
tApply (Transform ranges) = go ranges
  where
    go [] s = s
    go (r:rs) s =
      case rApply r s of
        Just s' -> s'
        Nothing -> go rs s

data Problem =
  Problem
    { pSeeds      :: [Int]
    , pTransforms :: [Transform]
    }
  deriving (Ord, Eq, Show)

parser =
  lineGroupsP &* unconsP &*
  (uncurry Problem <$>
   ((singleP &* pureP (Text.drop 7) &* integersP " ") &=
    traverseP
      (pureP tail &*
       (Transform <$>
        traverseP (tsplitP " " &* ap3P Range integerP integerP integerP)))))

part1 (Problem seeds transform) = minimum $ map applyAll seeds
  where
    applyAll s = foldl' (flip tApply) s transform

tasks =
  Tasks
    2023
    5
    (CodeBlock 0)
    parser
    [ Assert "rApply" (rApply (Range 50 98 2) 98) (Just 50)
    , Assert "rApply2" (rApply (Range 52 50 48) 79) (Just 81)
    , Task part1 35
    ]
