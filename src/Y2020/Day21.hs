module Y2020.Day21
  ( tasks
  ) where

import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Data.Text    as Text

import           GHC.Generics (Generic)

import           AOC
import           Path
import           Search
import           Utils

newtype A =
  A
    { getA :: Text
    }
  deriving (Eq, Ord, Show)

instance Hashable A where
  hashWithSalt s = hashWithSalt s . getA

newtype I =
  I
    { getI :: Text
    }
  deriving (Eq, Ord, Show)

instance Hashable I where
  hashWithSalt s = hashWithSalt s . getI

type Food = (Set I, Set A)

parseFood :: Parser Text Food
parseFood = tsplitP " (contains " &* parseIngredients &+ parseAllergens
  where
    parseAllergens = Set.fromList . map A <$> pureP (terase ")") &* tsplitP ", "
    parseIngredients = Set.fromList . map I <$> wordsP

type Input = [Food]

type Solution = Map A I

type Possibilities = Map A (Set I)

possibilities :: Input -> Possibilities
possibilities = foldr1 go . map (uncurry mk)
  where
    mk :: Set I -> Set A -> Possibilities
    mk is as = Map.fromList [(a, is) | a <- Set.toList as]
    go :: Possibilities -> Possibilities -> Possibilities
    go = Map.unionWith Set.intersection

solveAll :: Input -> [Solution]
solveAll = decideMappingAll . possibilities

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

part2 :: Input -> Text
part2 foods =
  case solveAll foods of
    [solution] -> Text.intercalate "," $ map getI $ Map.elems solution
    ms         -> error $ "multiple solutions: " ++ show ms

tasks =
  Tasks
    2020
    21
    (CodeBlock 0)
    (linesP &** parseFood)
    [Task part1 5, Task part2 "mxmxvkd,sqjhc,fvjkl"]
