module Y2023.Day05 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import qualified Ranges
import           Ranges    (Ranges)
import           Utils

str1 :: [Int] -> Ranges
str1 = foldr (\x -> Ranges.union (Ranges.singleton x $ succ x)) Ranges.empty

str2 :: [Int] -> Ranges
str2 []      = Ranges.empty
str2 (x:l:r) = Ranges.union (Ranges.singleton x $ x + l) (str2 r)

data Offset = Offset
  { oDst :: Int
  , oSrc :: Int
  , oLen :: Int
  } deriving (Ord, Eq, Show)

oSrcRange :: Offset -> Ranges
oSrcRange o = Ranges.singleton (oSrc o) (oSrc o + oLen o)

oIntersect :: Offset -> Ranges -> Ranges
oIntersect o = Ranges.fromList . go . Ranges.toList
  where
    go [] = []
    go ((a, b):rs)
      | b <= oSrc o = go rs
      | oSrc o + oLen o < a = []
      | otherwise = (max a (oSrc o), min b (oSrc o + oLen o)) : go rs

oShift :: Offset -> Ranges -> Ranges
oShift o = Ranges.fromList . map shift . Ranges.toList
  where
    shift (a, b) = (a + c, b + c)
    c = oDst o - oSrc o

newtype Transform = Transform
  { tOffsets :: [Offset]
  } deriving (Ord, Eq, Show)

tApply :: Transform -> Ranges -> Ranges
tApply (Transform offsets) d = go offsets d Ranges.empty
  where
    go [] d rd = Ranges.union d rd
    go (o:os) d rd =
      go os (Ranges.subtract or d) (Ranges.union (oShift o $ oIntersect o d) rd)
      where
        or = oSrcRange o

tApplyAll :: [Transform] -> Ranges -> Ranges
tApplyAll transforms s = foldl' (flip tApply) s transforms

data Problem = Problem
  { pSeeds      :: [Int]
  , pTransforms :: [Transform]
  } deriving (Ord, Eq, Show)

parser =
  lineGroupsP
    &* unconsP
    &* (uncurry Problem
          <$> ((singleP &* pureP (Text.drop 7) &* integersP " ")
                 &= traverseP
                      (pureP tail
                         &* (Transform
                               <$> traverseP
                                     (tsplitP " "
                                        &* ap3P
                                             Offset
                                             integerP
                                             integerP
                                             integerP)))))

plantAll :: ([Int] -> Ranges) -> Problem -> Int
plantAll str (Problem seeds transforms) =
  fromJustE "no minimum" $ Ranges.minimum $ tApplyAll transforms $ str seeds

part1 = plantAll str1

part2 = plantAll str2

tasks =
  Tasks
    2023
    5
    (CodeBlock 0)
    parser
    [ Assert "dMinimum" (Just 20) (Ranges.minimum $ Ranges.singleton 20 30)
    , AssertExample
        "tApply 1 1"
        (str1 [81])
        (\(Problem _ (t1:_)) -> tApply t1 $ str1 [79])
    , AssertExample
        "tApply 1"
        (str1 [81, 14, 57, 13])
        (\(Problem s (t1:_)) -> tApply t1 $ str1 s)
    , Task part1 35
    , Task part2 46
    ]
