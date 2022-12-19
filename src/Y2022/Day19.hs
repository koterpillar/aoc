module Y2022.Day19 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text     as Text

import           AOC
import           Data.Foldable (foldrM)
import           Utils

data Material
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Eq, Ord, Show, Enum, Bounded)

materialP :: Parser Text Material
materialP = choiceEBP ["ore", "clay", "obsidian", "geode"]

type Inventory = Map Material Int

inventoryP :: Parser Text Inventory
inventoryP =
  Map.fromList <$>
  tsplitP " and " &** (swap <$> wordsP &* integerP &+ materialP)

data Blueprint =
  Blueprint
    { bID    :: Int
    , bCosts :: Map Material Inventory
    }
  deriving (Eq, Ord, Show)

recipeP :: Parser Text (Material, Inventory)
recipeP =
  tsplitP " robot costs " &* (pureP (terase "Each ") &* materialP) &+
  (pureP (terase ".") &* inventoryP)

blueprintP :: Parser Text Blueprint
blueprintP =
  uncurry Blueprint <$>
  tsplitP ": " &* (pureP (terase "Blueprint ") &* integerP) &+
  (Map.fromList <$> tsplitP ". " &** recipeP)

parser :: Parser Text [Blueprint]
parser =
  pureP (treplace "\n  " " ") &* linesP &* pureP (filter $ not . Text.null) &**
  blueprintP

maxGeodes :: Blueprint -> Int
maxGeodes b = go b 24 stInit

data St =
  St
    { stRobots    :: Inventory
    , stResources :: Inventory
    }
  deriving (Eq, Ord, Show)

stInit :: St
stInit = St {..}
  where
    stRobots = Map.singleton Ore 1
    stResources = Map.fromList $ zip enumerate $ repeat 0

stTick :: St -> St
stTick st = st {stResources = newResources}
  where
    newResources = Map.mapWithKey incr $ stResources st
    incr m n = n + fromMaybe 0 (Map.lookup m $ stRobots st)

stTake :: Material -> Int -> St -> Maybe St
stTake m q s =
  if available >= q
    then Just $ s {stResources = Map.adjust (subtract q) m res}
    else Nothing
  where
    res = stResources s
    available = fromMaybe 0 $ Map.lookup m res

stConstruct :: Blueprint -> Material -> St -> Maybe St
stConstruct b m st = foldrM (uncurry stTake) st1 $ Map.toList bom
  where
    st1 = st {stRobots = Map.insertWith (+) m 1 $ stRobots st}
    bom = mapLookupE "bom" m $ bCosts b

go :: Blueprint -> Int -> St -> Int
go _ 0 St {..} = fromMaybe 0 $ Map.lookup Geode stResources
go b time st =
  maximum $
  map (go b (time - 1) . stTick) $
  catMaybes [stConstruct b robot st | robot <- reverse enumerate] ++ [st]

part1 bps = sum [bID bp * maxGeodes bp | bp <- bps]

tasks = Tasks 2022 19 (CodeBlock 0) parser [Task part1 33]
