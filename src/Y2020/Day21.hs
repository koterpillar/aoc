{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Y2020.Day21 (tasks) where

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

sStart :: Input -> Search
sStart = Search Map.empty

picks :: (Ord a, Ord b) => [a] -> Set b -> [Map a b]
picks [] bs = [Map.empty]
picks (a:ar) bs = do
  b <- Set.toList bs
  let br = Set.delete b bs
  Map.insert a b <$> picks ar br

moves :: Search -> Maybe [Search]
moves (Search _ []) = Nothing
moves (Search a2i ((is, as):rest)) =
  Just $ do
    for_ (Map.toList a2i) $ \(a, i) ->
      when (a `Set.member` as) $ guard $ i `Set.member` is
    let knownIs = setMapMaybe (`Map.lookup` a2i) as
    let unknownIs = Set.difference is knownIs
    let unknownAs = Set.difference as (Map.keysSet a2i)
    m <- picks (Set.toList unknownAs) unknownIs
    pure $ Search (Map.union m a2i) rest

solveAll :: Input -> [Solution]
solveAll = go . sStart
  where
    go s =
      case moves s of
        Nothing -> [sSolution s]
        Just ss -> concatMap go ss

safeIs :: Input -> [Solution] -> Set I
safeIs foods a2is = Set.difference is $ Set.unions $ map mapElemsSet a2is
  where
    is = Set.unions $ map fst foods

part1 :: Input -> Int
part1 foods = sum $ map (countIf isSafe . Set.toList . fst) foods
  where
    a2is = map traceShowId $ solveAll foods
    is = Set.unions $ map fst foods
    isSafe i = Set.member i $ safeIs foods a2is

tasks = Tasks 2020 21 (CodeBlock 0) (linesP &** parseFood) [Task part1 5]
