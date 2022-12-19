{-# LANGUAGE Strict #-}

module Y2022.Day19
  ( tasks
  ) where

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

maxGeodes :: Int -> Blueprint -> Int
maxGeodes deadline b = evalState (go b st) latticeEmpty
  where
    st = stInit deadline

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

stInit :: Int -> St
stInit stTime = St {..}
  where
    stRobots = Map.insert Ore 1 emptyInventory
    stResources = emptyInventory

stTick :: St -> St
stTick st = st {stResources = newResources, stTime = pred $ stTime st}
  where
    newResources = Map.mapWithKey incr $ stResources st
    incr m = (+) $ stRobot m st

stCost :: Blueprint -> Material -> Material -> Int
stCost b r m = fromMaybe 0 $ Map.lookup m $ mapLookupE "stCost" r $ bCosts b

-- | How many robots of a given type do we need to be able to not be restricted by this type?
bMaxFeasible :: Blueprint -> Material -> Int
bMaxFeasible _ Geode = 50
bMaxFeasible b r =
  maximum [c | r' <- enumerate, r' /= r, let c = stCost b r' r, c > 0]

bLatestFeasible :: Blueprint -> Material -> Int
bLatestFeasible _ Geode = 1
bLatestFeasible b r = bLatestFeasible b r' + 2
  where
    r' = pred r

stCanConstruct :: Blueprint -> St -> Material -> Bool
stCanConstruct b st r
  | stRobot r st >= bMaxFeasible b r = False
  | bLatestFeasible b r > stTime st = False
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
    consume m = subtract $ stCost b r m

go :: Blueprint -> St -> State Lattice Int
go b st
  | stTime st == 1 = pure $ stResource Geode st + stRobot Geode st
  | stTime st == 0 = pure $ stResource Geode st
  | otherwise =
    latticeInsertS st >>= \case
      False -> pure 0
      True -> do
        let canConstruct = filter (stCanConstruct b st) enumerate
        let st1 = stTick st
        let candidates = map (stConstruct b st1) canConstruct ++ [st1]
        maximum <$> traverse (go b) (take 4 candidates)

part1 = sum . map (qs . traceShowId)
  where
    qs bp = bID bp * traceF (prependShow "result") (maxGeodes 24 bp)

part2 = product . map (maxGeodes 32 . traceShowId) . take 3

tasks = Tasks 2022 19 (CodeBlock 0) parser [Task part1 33, Task part2 (56 * 62)]
