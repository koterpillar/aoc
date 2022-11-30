module Y2020.Day21 where

import qualified Data.Set as Set

import           AOC
import           Utils

newtype A =
  A Text
  deriving (Eq, Ord, Show)

newtype I =
  I Text
  deriving (Eq, Ord, Show)

type Food = (Set I, Set A)

parseFood :: Parser Text Food
parseFood = tsplitP " (contains " &* (parseIngredients &+ parseAllergens)
  where
    parseAllergens = Set.fromList . map A <$> pureP (terase ")") &* tsplitP ", "
    parseIngredients = Set.fromList . map I <$> wordsP

part1 = error . show

tasks = Tasks 2020 21 (CodeBlock 0) (linesP &** parseFood) [Task part1 ()]
