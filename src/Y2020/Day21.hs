{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Y2020.Day21 where

import qualified Data.Map     as Map
import qualified Data.Set     as Set

import           GHC.Generics (Generic)

import           AOC
import           Path
import           Utils

newtype A =
  A Text
  deriving (Eq, Ord, Generic, Hashable, Show)

newtype I =
  I Text
  deriving (Eq, Ord, Generic, Hashable, Show)

type Food = (Set I, Set A)

parseFood :: Parser Text Food
parseFood = tsplitP " (contains " &* (parseIngredients &+ parseAllergens)
  where
    parseAllergens = Set.fromList . map A <$> pureP (terase ")") &* tsplitP ", "
    parseIngredients = Set.fromList . map I <$> wordsP

type Input = [Food]

type Solution = Map A I

data Search =
  Search
    { sSolution :: Solution
    , sRemF     :: [Food]
    }
  deriving (Eq, Ord, Generic, Hashable, Show)

picks :: (Ord a, Ord b) => [a] -> Set b -> [Map a b]
picks [] bs = [Map.empty]
picks (a:ar) bs = do
  b <- Set.toList bs
  let br = Set.delete b bs
  Map.insert a b <$> picks ar br

moves :: Search -> [Search]
moves (Search _ []) = []
moves (Search a2i ((is, as):rest)) = do
  let knownIs = Set.fromList $ mapMaybe (`Map.lookup` a2i) (Set.toList as)
  let unknownIs = Set.difference is knownIs
  let unknownAs = Set.difference as (Set.fromList $ Map.keys a2i)
  m <- picks (Set.toList unknownAs) unknownIs
  pure $ Search (Map.union m a2i) rest

solve :: Input -> Solution
solve foods =
  sSolution $
  lastE "solve: empty route" $
  fromJustE "solve: no solution" $
  dfs (hashSetFromList . moves) distanceToGoal start
  where
    distanceToGoal = length . sRemF
    start = Search Map.empty foods

part1 :: Input -> Int
part1 foods = sum $ map (countIf isSafe . Set.toList . fst) foods
  where
    a2i = solve foods
    is = Set.unions $ map fst foods
    safeIs = Set.difference is $ Set.fromList $ Map.elems a2i
    isSafe i = Set.member i safeIs

tasks = Tasks 2020 21 (CodeBlock 0) (linesP &** parseFood) [Task part1 5]
