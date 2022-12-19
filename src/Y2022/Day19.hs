module Y2022.Day19 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Material
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Eq, Ord, Show, Enum, Bounded)

materialP :: Parser Text Material
materialP = choiceEBP ["ore", "clay", "obsidian", "geode"]

type Cost = Map Material Int

costP :: Parser Text Cost
costP =
  Map.fromList <$>
  tsplitP " and " &** (swap <$> wordsP &* integerP &+ materialP)

data Blueprint =
  Blueprint
    { bID    :: Int
    , bCosts :: Map Material Cost
    }
  deriving (Eq, Ord, Show)

recipeP :: Parser Text (Material, Cost)
recipeP =
  tsplitP " robot costs " &* (pureP (terase "Each ") &* materialP) &+
  (pureP (terase ".") &* costP)

blueprintP :: Parser Text Blueprint
blueprintP =
  uncurry Blueprint <$>
  tsplitP ": " &* (pureP (terase "Blueprint ") &* integerP) &+
  (Map.fromList <$> tsplitP ". " &** recipeP)

parser :: Parser Text [Blueprint]
parser =
  pureP (treplace "\n  " " ") &* linesP &* pureP (filter $ not . Text.null) &**
  blueprintP

part1 = error . show

tasks = Tasks 2022 19 (CodeBlock 0) parser [Task part1 33]
