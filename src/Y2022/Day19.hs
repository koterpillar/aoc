{-# LANGUAGE Strict #-}

module Y2022.Day19 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Data.Foldable              (foldrM)
import           Utils

import           Y2022.Lattice

data Material
  = Geode
  | Obsidian
  | Clay
  | Ore
  deriving (Eq, Ord, Show, Enum, Bounded)

materialP :: Parser Text Material
materialP = choiceEBP ["geode", "obsidian", "clay", "ore"]

type Inventory = Map Material Int

emptyInventory :: Inventory
emptyInventory = Map.fromList $ zip enumerate $ repeat 0

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
maxGeodes b = evalState (go b stInit) latticeEmpty

data St =
  St
    { stRobots    :: Inventory
    , stResources :: Inventory
    , stTime      :: Int
    }
  deriving (Eq, Ord, Show)

stResource :: Material -> St -> Int
stResource m = mapLookupE "stResource" m . stResources

stRobot :: Material -> St -> Int
stRobot r = mapLookupE "stRobot" r . stRobots

instance Positionable St where
  latticePosition St {..} =
    Map.elems stResources ++ Map.elems stRobots ++ [stTime]

stInit :: St
stInit = St {..}
  where
    stRobots = Map.insert Ore 1 emptyInventory
    stResources = emptyInventory
    stTime = 24

stTick :: St -> St
stTick st = st {stResources = newResources, stTime = pred $ stTime st}
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

stCost :: Blueprint -> Material -> Material -> Int
stCost b r m = fromMaybe 0 $ Map.lookup m $ mapLookupE "stCost" r $ bCosts b

stCanConstruct :: Blueprint -> St -> Material -> Bool
stCanConstruct b st r
  | r == Ore &&
      stRobot Ore st >= maximum [stCost b r' Ore | r' <- enumerate, r' /= Ore] =
    False
  | otherwise = all enough enumerate
  where
    enough m = stResource m st >= stCost b r m

stConstruct :: Blueprint -> St -> Material -> St
stConstruct b st r =
  st
    { stRobots = Map.adjust succ r $ stRobots st
    , stResources = Map.mapWithKey consume $ stResources st
    }
  where
    consume m q = q - stCost b r m

go :: Blueprint -> St -> State Lattice Int
go b st
  | stTime st == 0 = pure $ fromMaybe 0 $ Map.lookup Geode $ stResources st
  | otherwise =
    latticeInsertS st >>= \case
      False -> pure 0
      True -> do
        let canConstruct = filter (stCanConstruct b st) enumerate
        let st1 = stTick st
        let candidates = map (stConstruct b st1) canConstruct ++ [st1]
        maximum <$> traverse (go b) (take 2 candidates)

part1 = sum . map (qs . traceShowId)
  where
    qs bp = bID bp * traceF (prependShow "result") (maxGeodes bp)

tasks = Tasks 2022 19 (CodeBlock 0) parser [Task part1 33]
