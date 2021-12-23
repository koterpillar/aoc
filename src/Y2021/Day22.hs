{-# LANGUAGE ExplicitForAll #-}

module Y2021.Day22 where

import           Control.Monad (foldM)

import qualified Data.Map      as Map

import           AOC
import           Bit
import           Utils

data Range =
  Range
    { rangeGreaterOrEqual :: !(Maybe Int)
    , rangeLessThan       :: !(Maybe Int)
    }
  deriving (Ord, Eq)

instance Show Range where
  show (Range (Just a) (Just b)) = "Range [" <> show a <> ".." <> show b <> ")"
  show (Range (Just a) Nothing) = "Range [" <> show a <> ".."
  show (Range Nothing (Just b)) = "Range " <> show b <> "..)"
  show (Range Nothing Nothing) = "Range .."

nullRange :: Range
nullRange = Range Nothing Nothing

mkRangeGE :: Int -> Range
mkRangeGE v = Range (Just v) Nothing

mkRangeLT :: Int -> Range
mkRangeLT v = Range Nothing (Just v)

mkRange' :: Maybe Int -> Maybe Int -> Maybe Range
mkRange' (Just a) (Just b)
  | a < b = Just $ Range (Just a) (Just b)
  | otherwise = Nothing
mkRange' a b = Just $ Range a b

mkRange :: Int -> Int -> Maybe Range
mkRange a b
  | a < b = Just $ Range (Just a) (Just b)
  | otherwise = Nothing

rangeLength :: Range -> Maybe Int
rangeLength (Range a b) = liftA2 (-) b a

rangeIntersect :: Range -> Range -> Maybe Range
rangeIntersect (Range a1 b1) (Range a2 b2) = mkRange' (max a1 a2) (min b1 b2)

data Axis
  = AX
  | AY
  | AZ
  deriving (Ord, Eq, Show)

allAxis :: [Axis]
allAxis = [AX, AY, AZ]

type Cuboid = Map Axis Range

cRange :: Axis -> Cuboid -> Range
cRange axis cs = fromMaybe nullRange $ Map.lookup axis cs

cvolume :: Cuboid -> Maybe Int
cvolume cs = product <$> traverse (rangeLength . (`cRange` cs)) allAxis

cRanges :: Cuboid -> [(Axis, Range)]
cRanges cs = [(axis, cRange axis cs) | axis <- allAxis]

splitRangeAt :: Int -> Range -> (Maybe Range, Maybe Range)
splitRangeAt c r =
  (rangeIntersect r (mkRangeLT c), rangeIntersect r (mkRangeGE c))

data DecisionTree value
  = Value value
  | Branch Axis Int (DecisionTree value) (DecisionTree value)
  deriving (Ord, Eq, Show)

dtCount :: Eq value => value -> DecisionTree value -> Int
dtCount v = dtCountIn v Map.empty

dtCountIn :: Eq value => value -> Cuboid -> DecisionTree value -> Int
dtCountIn v cs (Value v')
  | v == v' =
    fromJustE ("infinite region while counting: " <> show cs) $ cvolume cs
  | otherwise = 0
dtCountIn v cs (Branch axis c yes no) = sum $ catMaybes [yes', no']
  where
    r = cRange axis cs
    (rYes, rNo) = splitRangeAt c r
    csWith r = Map.insert axis r cs
    yes' = dtCountIn v <$> fmap csWith rYes <*> pure yes
    no' = dtCountIn v <$> fmap csWith rNo <*> pure no

cToTree :: value -> Cuboid -> DecisionTree value -> DecisionTree value
cToTree v cs rest = foldr (uncurry go) rest $ cRanges cs
  where
    go axis (Range (Just a) b) next =
      Branch axis a next $ go axis (Range Nothing b) next
    go axis (Range Nothing (Just b)) next = Branch axis b (Value v) next
    go axis (Range Nothing Nothing) next = Value v

dtSet :: value -> Cuboid -> DecisionTree value -> DecisionTree value
dtSet v cs (Branch axis c yes no) = Branch axis c yes' no'
  where
    r = cRange axis cs
    (rYes, rNo) = splitRangeAt c r
    csWith r = Map.insert axis r cs
    yes' = foldr (dtSet v . csWith) yes rYes
    no' = foldr (dtSet v . csWith) no rNo
dtSet v cs prim = cToTree v cs prim

part1cuboid :: Cuboid
part1cuboid =
  Map.fromList [(axis, Range (Just (-50)) (Just 51)) | axis <- allAxis]

initial :: DecisionTree Bit
initial = Value O

applyInput :: Input -> DecisionTree Bit -> DecisionTree Bit
applyInput input initial = foldl (flip $ uncurry dtSet) initial input

part1 :: Input -> Int
part1 input = dtCountIn I part1cuboid $ applyInput input initial

example0 :: Input
example0 = [(I, mkc r1), (I, mkc r2), (O, mkc r3), (I, mkc r4)]
  where
    mkc r = Map.fromList [(a, r) | a <- allAxis]
    mkr a b = fromJustE "mkr" $ mkRange a (b + 1)
    r1 = mkr 10 12
    r2 = mkr 11 13
    r3 = mkr 9 11
    r4 = mkr 10 10

tasks =
  Tasks
    2021
    22
    parse
    [ Assert "count in one cube" (101 ^ 3) $ dtCountIn I part1cuboid $ Value I
    , Assert "count example 0 step 1" 27 $
      dtCount I $ traceShowId $ applyInput (take 1 example0) initial
    , Task part1 590784
    ]

type Input = [(Bit, Cuboid)]

parse :: Parser Text Input
parse = linesP &** (wordsP &* pairP &* (instrP &= cuboidP))

instrP :: Parser Text Bit
instrP = choiceP [("on", I), ("off", O)]

axisP :: Parser Text Axis
axisP = choiceP [("x", AX), ("y", AY), ("z", AZ)]

cuboidP :: Parser Text Cuboid
cuboidP =
  tsplitP "," &**
  (tsplitP "=" &* pairP &*
   (axisP &= (tsplitP ".." &* pairP &* (integerP &= integerP)))) &*
  pureP mkCuboid

mkCuboid :: [(Axis, (Int, Int))] -> Cuboid
mkCuboid = Map.fromList . map (second $ \(a, b) -> Range (Just a) (Just b))
