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

stConstruct :: Blueprint -> Material -> St -> Maybe St
stConstruct b m st = foldrM (uncurry stTake) st1 $ Map.toList bom
  where
    st1 = st {stRobots = Map.insertWith (+) m 1 $ stRobots st}
    bom = mapLookupE "bom" m $ bCosts b

go :: Blueprint -> St -> State Lattice Int
go b st
  | stTime st == 0 = pure $ fromMaybe 0 $ Map.lookup Geode $ stResources st
  | otherwise = do
    st' <- gets (`latticeInsert` st)
    case st' of
      Nothing -> do
        when (stTime st > 5) $ traceM $ prependShow "bad " st
        pure 0
      Just st1 -> do
        when (stTime st > 5) $ traceM $ prependShow "good" st
        put st1
        let candidates =
              map stTick $
              catMaybes [stConstruct b robot st | robot <- enumerate] ++ [st]
        maximum <$> traverse (go b) candidates

part1 bps = sum [bID bp * maxGeodes bp | bp <- bps]

tasks = Tasks 2022 19 (CodeBlock 0) parser [Task part1 33]
