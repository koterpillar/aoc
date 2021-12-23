{-# LANGUAGE ExplicitForAll #-}

module Y2021.Day22 where

import           Control.Monad (foldM)

import qualified Data.Map      as Map

import           AOC
import           Bit
import           Utils

type Range = (Int, Int)

mkRange :: Int -> Int -> Maybe Range
mkRange a b
  | a >= b = Just (a, b)
  | otherwise = Nothing

rangeLength :: Range -> Int
rangeLength (a, b) = b - a + 1

rangeIntersect :: Range -> Range -> Maybe Range
rangeIntersect (a1, b1) (a2, b2) = mkRange (max a1 a2) (min b1 b2)

-- a -> b -> (a - b)
rangeDiff :: Range -> Range -> [Range]
rangeDiff a@(a1, a2) b =
  case rangeIntersect a b of
    Just (c1, c2) -> mapMaybe (uncurry mkRange) [(a1, c1 - 1), (c2 + 1, a2)]
    Nothing       -> [a]

data DecisionTree value
  = Value value
  | Branch Axis (Map Range (DecisionTree value))
  deriving (Ord, Eq, Show)

data Axis
  = AX
  | AY
  | AZ
  deriving (Ord, Eq, Show)

allAxis :: [Axis]
allAxis = [AX, AY, AZ]

type Range3 = Map Axis Range

r3volume :: Range3 -> Maybe Int
r3volume r3 =
  product <$> traverse (fmap rangeLength . (`Map.lookup` r3)) allAxis

r3ForAxis :: Functor f => Axis -> (Maybe Range -> f Range) -> Range3 -> f Range3
r3ForAxis a f m =
  let fr = f (Map.lookup a m)
   in fmap (\r -> Map.insert a r m) fr

r3intersectAxis :: Axis -> Range -> Range3 -> Maybe Range3
r3intersectAxis a r =
  r3ForAxis a $ \case
    Nothing -> Just r
    Just r' -> rangeIntersect r r'

r3DiffAxis :: Axis -> Range -> Range3 -> [Range3]
r3DiffAxis a r =
  r3ForAxis a $ \case
    Nothing -> []
    Just r' -> rangeDiff r' r

r3intersect :: Range3 -> Range3 -> Maybe Range3
r3intersect r1 r2 = foldM (flip $ uncurry r3intersectAxis) r1 (Map.toList r2)

dtCount :: Eq value => value -> DecisionTree value -> Int
dtCount target = go Map.empty
  where
    go r3 (Value v)
      | v == target =
        fromJustE ("infinite region while counting: " <> show r3) $ r3volume r3
      | otherwise = 0
    go r3 (Branch axis branches) =
      sum
        [ go r3' b
        | (r, b) <- Map.toList branches
        , r3' <- maybeToList (r3intersectAxis axis r r3)
        ]

dtSet :: value -> Range3 -> DecisionTree value -> DecisionTree value
dtSet = _

part1 = dtCount I . foldl (flip $ uncurry dtSet) (Value O)

tasks = Tasks 2021 22 parse [Task part1 590784]

type Input = [(Bit, Range3)]

parse :: Parser Text Input
parse = linesP &** (wordsP &* pairP &* (instrP &= cuboidP))

instrP :: Parser Text Bit
instrP = choiceP [("on", I), ("off", O)]

axisP :: Parser Text Axis
axisP = choiceP [("x", AX), ("y", AY), ("z", AZ)]

cuboidP :: Parser Text Range3
cuboidP =
  tsplitP "," &**
  (tsplitP "=" &* pairP &*
   (axisP &= (tsplitP "=" &* pairP &* (integerP &= integerP)))) &*
  pureP Map.fromList
