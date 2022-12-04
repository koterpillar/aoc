{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow

import           Data.Foldable

import           Data.Ord

import           Data.List.Split
import           Data.List.Utils

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Maybe

import           Data.Monoid

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Graph
import           Path
import           Utils

type Port = Int

data Component =
  Component !Port !Port
  deriving (Ord, Eq)

instance Show Component where
  show (Component p1 p2) = show p1 ++ "/" ++ show p2

readComps :: IO [Component]
readComps = map parseComp <$> readLines

parseComp :: String -> Component
parseComp str =
  let [p1, p2] = splitOn "/" str
   in Component (read p1) (read p2)

example :: [Component]
example =
  map parseComp ["0/2", "2/2", "2/3", "3/4", "3/5", "0/1", "10/1", "9/10"]

data BridgeState =
  BridgeState
    { bsBridge    :: [Component]
    , bsLastPort  :: Port
    , bsRemaining :: Set Component
    }
  deriving (Ord, Eq, Show)

bsInitial :: [Component] -> BridgeState
bsInitial cs = BridgeState {..}
  where
    bsBridge = []
    bsLastPort = 0
    bsRemaining = Set.fromList cs

bsMoves :: BridgeState -> [Component]
bsMoves = Set.toList . bsRemaining

bsApply :: Component -> BridgeState -> Maybe BridgeState
bsApply c@(Component p1 p2) old
  | p1 == bsLastPort old = go p2
  | p2 == bsLastPort old = go p1
  | otherwise = Nothing
  where
    go newPort =
      Just
        BridgeState
          { bsBridge = bsBridge old ++ [c]
          , bsLastPort = newPort
          , bsRemaining = Set.delete c $ bsRemaining old
          }

bsTree :: BridgeState -> Tree Component BridgeState
bsTree = moveTree bsMoves bsApply

pathStrength :: [Component] -> Int
pathStrength = sum . map (\(Component c1 c2) -> c1 + c2)

bsStrength :: BridgeState -> Int
bsStrength = pathStrength . bsBridge

bsStrength' :: BridgeState -> Int
bsStrength' bs
  | Set.null (bsRemaining bs) = bsStrength bs
  | otherwise = -1

bsLength :: BridgeState -> Int
bsLength bs = length (bsBridge bs)

strongestBridge :: Foldable t => t BridgeState -> BridgeState
strongestBridge = maximumBy $ comparing bsStrength'

longestBridge :: Foldable t => t BridgeState -> BridgeState
longestBridge = maximumBy $ comparing (bsLength &&& bsStrength)
