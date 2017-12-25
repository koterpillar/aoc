{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

import Data.List.Utils

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Graph
import Utils

type Position = Int

type Value = Int

type TMState = Char

data Direction
  = TMLeft
  | TMRigh
  deriving (Eq, Ord, Show)

data TM = TM
  { _tmPosition :: !Position
  , _tmTape :: !(Map Position Value)
  , _tmState :: !TMState
  , _tmSteps :: !Int
  } deriving (Eq, Ord)

makeLenses ''TM

instance Show TM where
  show tm =
    (tm ^. tmState) : ' ' : show (tm ^. tmSteps) ++ " " ++
    concatMap showCell [tapeMin - 2 .. tapeMax + 2]
    where
      cur = tm ^. tmPosition
      dirty = Map.keys $ tm ^. tmTape
      tapeMin = minimum $ cur : dirty
      tapeMax = maximum $ cur : dirty
      showCell pos
        | cur == pos = ['[', showCell' pos, ']']
        | otherwise = [showCell' pos]
      showCell' pos =
        case tm ^. tmTape . at pos . non 0 of
          0 -> '0'
          1 -> '1'

tmCurrent :: Lens' TM Value
tmCurrent = lens getCurrent setCurrent
  where
    getCurrent tm = tm ^. tmTape . at (tm ^. tmPosition) . non 0
    setCurrent tm v = tm & tmTape . at (tm ^. tmPosition) . non 0 .~ v

tmAtRelative :: Int -> Lens' TM Value
tmAtRelative offset = lens getRelative setRelative
  where
    getRelative tm = tm ^. tmTape . at (tm ^. tmPosition + offset) . non 0
    setRelative tm v = tm & tmTape . at (tm ^. tmPosition + offset) . non 0 .~ v

type Program = Map (TMState, Value) (Value, Direction, TMState)

step :: Program -> TM -> TM
step prg old =
  old & tmCurrent .~ newValue & tmPosition %~ move dir & tmState .~ newState &
  tmSteps +~
  1
  where
    (newValue, dir, newState) =
      fromJust $ Map.lookup (old ^. tmState, old ^. tmCurrent) prg
    move TMLeft x = x - 1
    move TMRigh x = x + 1

tmNew :: TM
tmNew = TM {_tmPosition = 0, _tmTape = Map.empty, _tmState = 'A', _tmSteps = 0}

tmChecksum :: TM -> Int
tmChecksum tm = sum $ Map.elems $ tm ^. tmTape

example :: Program
example =
  Map.fromList
    [ (('A', 0), (1, TMRigh, 'B'))
    , (('A', 1), (0, TMLeft, 'B'))
    , (('B', 0), (1, TMLeft, 'A'))
    , (('B', 1), (1, TMRigh, 'A'))
    ]

real :: Program
real =
  Map.fromList
    [ (('A', 0), (1, TMRigh, 'B'))
    , (('A', 1), (0, TMLeft, 'C'))
    , (('B', 0), (1, TMLeft, 'A'))
    , (('B', 1), (1, TMRigh, 'D'))
    , (('C', 0), (1, TMRigh, 'A'))
    , (('C', 1), (0, TMLeft, 'E'))
    , (('D', 0), (1, TMRigh, 'A'))
    , (('D', 1), (0, TMRigh, 'B'))
    , (('E', 0), (1, TMLeft, 'F'))
    , (('E', 1), (1, TMLeft, 'C'))
    , (('F', 0), (1, TMRigh, 'D'))
    , (('F', 1), (1, TMRigh, 'A'))
    ]
