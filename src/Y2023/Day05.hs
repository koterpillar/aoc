module Y2023.Day05 where

import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Data.Text    as Text

import           AOC
import           GHC.TypeLits (Div)
import           Utils

newtype Division =
  Division
    { dRanges :: [(Int, Int)]
    }
  deriving (Ord, Eq, Show)

mkDivision :: Text -> [(Int, Int)] -> Division
mkDivision w = Division . map (mkRange w)

mkRange :: Text -> (Int, Int) -> (Int, Int)
mkRange w (a, b)
  | a == b = terror $ "mkRange: " <> w <> ": empty interval " <> tshow (a, b)
  | a > b = terror $ "mkRange: " <> w <> ": negative interval " <> tshow (a, b)
  | otherwise = (a, b)

dEmpty :: Division
dEmpty = Division []

dSingleton :: (Int, Int) -> Division
dSingleton = Division . (: [])

dAdd1 :: (Int, Int) -> Division -> Division
dAdd1 r (Division rs) = mkDivision "dAdd1" $ go r rs
  where
    go r [] = [r]
    go (a, b) ((c, d):rs)
      | b < c = (a, b) : (c, d) : rs
      | d < a = (c, d) : go (a, b) rs
      | otherwise = go (min a c, max b d) rs

dAdd :: Division -> Division -> Division
dAdd d = foldr dAdd1 d . dRanges

dRemove :: (Int, Int) -> Division -> Division
dRemove (a, b) (Division rs) = mkDivision "dRemove" $ go rs
  where
    go [] = []
    go ((c, d):rs)
      | b <= c = (c, d) : rs
      | d <= a = (c, d) : go rs
      | a <= c && d <= b = go rs
      | a <= c = mkRange "dRemove 1" (b, d) : go rs
      | d <= b = mkRange "dRemove 2" (c, a) : go rs
      | otherwise =
        mkRange "dRemove 3a" (c, a) : mkRange "dRemove 3b" (b, d) : go rs

dMinimum :: Division -> Maybe Int
dMinimum = fmap fst . listToMaybe . dRanges

str1 :: [Int] -> Division
str1 = foldr (\x -> dAdd1 (x, x + 1)) dEmpty

str2 :: [Int] -> Division
str2 []      = dEmpty
str2 (x:l:r) = dAdd1 (x, x + l) (str2 r)

data Offset =
  Offset
    { oDst :: Int
    , oSrc :: Int
    , oLen :: Int
    }
  deriving (Ord, Eq, Show)

oRemove :: Offset -> Division -> Division
oRemove o = dRemove (oSrc o, oSrc o + oLen o)

oIntersect :: Offset -> Division -> Division
oIntersect o (Division rs) = mkDivision "oIntersect" $ go rs
  where
    go [] = []
    go ((a, b):rs)
      | b <= oSrc o = go rs
      | oSrc o + oLen o < a = []
      | otherwise = (max a (oSrc o), min b (oSrc o + oLen o)) : go rs

oShift :: Offset -> Division -> Division
oShift o (Division rs) = mkDivision "oShift" $ map shift rs
  where
    shift (a, b) = (a + c, b + c)
    c = oDst o - oSrc o

newtype Transform =
  Transform
    { tOffsets :: [Offset]
    }
  deriving (Ord, Eq, Show)

tApply :: Transform -> Division -> Division
tApply (Transform offsets) d = go offsets d dEmpty
  where
    go [] d rd     = dAdd d rd
    go (o:os) d rd = go os (oRemove o d) (dAdd (oShift o $ oIntersect o d) rd)

tApplyAll :: [Transform] -> Division -> Division
tApplyAll transforms s = foldl' (flip tApply) s transforms

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
        traverseP (tsplitP " " &* ap3P Offset integerP integerP integerP)))))

plantAll :: ([Int] -> Division) -> Problem -> Int
plantAll str (Problem seeds transforms) =
  fromJustE "no minimum" $ dMinimum $ tApplyAll transforms $ str seeds

part1 = plantAll str1

part2 = plantAll str2

tasks =
  Tasks
    2023
    5
    (CodeBlock 0)
    parser
    [ Assert "dMinimum" (Just 20) (dMinimum $ dAdd1 (20, 30) dEmpty)
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
