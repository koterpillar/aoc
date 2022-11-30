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
    , sRemA     :: Set A
    , sRemI     :: Set I
    }
  deriving (Eq, Ord, Generic, Hashable, Show)

picks :: Search -> [Search]
picks Search {..} = do
  let (i, ri) = Set.deleteFindMin sRemI
  a <- Set.toList sRemA
  pure $ Search (Map.insert a i sSolution) (Set.delete a sRemA) ri

valid1 :: Solution -> Food -> Bool
valid1 a2i (is, as) =
  all (`Set.member` is) $ mapMaybe (`Map.lookup` a2i) $ Set.toList as

valid :: Foldable t => Solution -> t Food -> Bool
valid solution = all $ valid1 solution

solve :: Input -> Solution
solve foods =
  sSolution $
  lastE "solve: empty route" $
  fromJustE "solve: no solution" $ dfs moves distanceToGoal start
  where
    distanceToGoal = length . sRemA . traceShowId
    start =
      Search Map.empty (Set.unions $ map snd foods) (Set.unions $ map fst foods)
    moves = hashSetFromList . filter ((`valid` foods) . sSolution) . picks

part1 :: Input -> Int
part1 foods = length noAllergens
  where
    a2i = solve foods
    is = Set.unions $ map fst foods
    noAllergens = Set.difference is $ Set.fromList $ Map.elems a2i

tasks = Tasks 2020 21 (CodeBlock 0) (linesP &** parseFood) [Task part1 5]
