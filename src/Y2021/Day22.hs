{-# LANGUAGE ExplicitForAll #-}

module Y2021.Day22 where

import           Control.Monad (foldM)

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
  show (Range a b) = "Range [" <> s' a <> ".." <> s' b <> ")"
    where
      s' (Just n) = show n
      s' Nothing  = "∞"

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
rangeIntersect (Range a1 b1) (Range a2 b2) =
  mkRange'
    (maybeMaximum $ catMaybes [a1, a2])
    (maybeMinimum $ catMaybes [b1, b2])

data Axis
  = AX
  | AY
  | AZ
  deriving (Ord, Eq, Bounded, Enum, Show)

allAxis :: [Axis]
allAxis = enumerate

type Cuboid = Map Axis Range

cRange :: Axis -> Cuboid -> Range
cRange axis cs = fromMaybe nullRange $ mapLookup axis cs

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
  deriving (Ord, Eq)

instance Show value => Show (DecisionTree value) where
  show (Value v) = show v
  show (Branch axis c yes no) =
    "{" <>
    show yes <>
    "|" <> tail (show axis) <> "=" <> show c <> "|" <> show no <> "}"

dtCount :: (Eq value, Show value) => value -> DecisionTree value -> Int
dtCount v = dtCountIn v mempty

dtCountIn ::
     (Eq value, Show value) => value -> Cuboid -> DecisionTree value -> Int
dtCountIn v cs (Value v')
  | v == v' =
    fromJustE
      ("infinite region while counting " <> show v <> " inside " <> show cs) $
    cvolume cs
  | otherwise = 0
dtCountIn v cs t@(Branch axis c yes no) = sum $ catMaybes [yes', no']
  where
    r = cRange axis cs
    (ryes, rno) =
      traceF
        (\yn ->
           "current range: " <>
           show r <> " split at: " <> show c <> " result: " <> show yn) $
      splitRangeAt c r
    csWith r' = mapInsert axis r' cs
    yes' = dtCountIn v <$> fmap csWith ryes <*> pure yes
    no' = dtCountIn v <$> fmap csWith rno <*> pure no

cToTree :: value -> Cuboid -> DecisionTree value -> DecisionTree value
cToTree v cs rest = go allAxis
  where
    go [] = Value v
    go (axis:as) = Branch axis a rest $ Branch axis b (go as) rest
      where
        (Range (Just a) (Just b)) = cRange axis cs

dtSet :: value -> Cuboid -> DecisionTree value -> DecisionTree value
dtSet v cs (Branch axis c yes no) = Branch axis c yes' no'
  where
    r = cRange axis cs
    (rYes, rNo) = splitRangeAt c r
    csWith r = mapInsert axis r cs
    yes' = foldr (dtSet v . csWith) yes rYes
    no' = foldr (dtSet v . csWith) no rNo
dtSet v cs prim = cToTree v cs prim

part1cuboid :: Cuboid
part1cuboid =
  mapFromList [(axis, Range (Just (-50)) (Just 51)) | axis <- allAxis]

initial :: DecisionTree Bit
initial = Value O

applyInput :: Input -> DecisionTree Bit -> DecisionTree Bit
applyInput input initial = foldl (flip $ uncurry dtSet) initial input

part1 :: Input -> Int
part1 input = dtCountIn I part1cuboid $ applyInput input initial

part2 :: Input -> Int
part2 input = dtCount I $ applyInput input initial

tasks =
  Tasks
    2021
    22
    (CodeBlock 2)
    parse
    [ Assert "split range" (Just $ mkRangeLT 10, Just $ mkRangeGE 10) $
      splitRangeAt 10 nullRange
    , Assert "count in one cube" (101 ^ 3) $ dtCountIn I part1cuboid $ Value I
    , Assert "count example 0 step 1" 27 $
      dtCount I $ traceShowId $ applyInput (take 1 example0) initial
    , Assert "count example 0 step 2" (27 + 19) $
      dtCount I $ traceShowId $ applyInput (take 2 example0) initial
    , Task part1 474140
    , Task part2 2758514936282235
    ]

type Input = [(Bit, Cuboid)]

example0 :: Input
example0 =
  [ (I, justParse cuboidP "x=10..12,y=10..12,z=10..12")
  , (I, mkc r2)
  , (O, mkc r3)
  , (I, mkc r4)
  ]
  where
    mkc r = mapFromList [(a, r) | a <- allAxis]
    mkr a b = fromJustE "mkr" $ mkRange a (b + 1)
    r1 = mkr 10 12
    r2 = mkr 11 13
    r3 = mkr 9 11
    r4 = mkr 10 10

parse :: Parser Text Input
parse = linesP &** wordsP &* instrP &+ cuboidP

instrP :: Parser Text Bit
instrP = choiceP [("on", I), ("off", O)]

axisP :: Parser Text Axis
axisP = choiceEBP ["x", "y", "z"]

cuboidP :: Parser Text Cuboid
cuboidP =
  (tsplitP "," &** tsplitP "=" &* axisP &+
   (tsplitP ".." &* integerP &+ integerP)) &*
  pureP mkCuboid

mkCuboid :: [(Axis, (Int, Int))] -> Cuboid
mkCuboid = mapFromList . map (second $ \(a, b) -> Range (Just a) (Just (b + 1)))
