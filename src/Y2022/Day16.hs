{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import           Control.Monad.State.Strict

import           Data.Foldable              (foldrM)

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Path
import           Utils

newtype VKey =
  VKey Text
  deriving (Eq, Ord)

instance Hashable VKey where
  hashWithSalt s (VKey k) = hashWithSalt s k

vAA :: VKey
vAA = VKey "AA"

instance Show VKey where
  show (VKey k) = Text.unpack k

data VData =
  VData
    { vFlow :: Int
    , vNext :: [(Int, VKey)]
    }
  deriving (Eq, Ord, Show)

mkVData :: Int -> [VKey] -> VData
mkVData f n = VData f $ zip (repeat 1) n

type Input = Map VKey VData

iValves :: Input -> [VKey]
iValves = mapFilterValues $ (> 0) . vFlow

parser :: Parser Text Input
parser =
  Map.fromList <$>
  linesP &** wordsP &* pureP tail &* unconsP &* (pureP VKey &= vdp)
  where
    vdp =
      pureP (drop 2) &*
      (uncurry mkVData <$>
       unconsP &*
       ((pureP (terase "rate=" . terase ";") &* integerP) &=
        (pureP (drop 4 . map (terase ",")) &** pureP VKey)))

-- | Cut out the given vertex, connecting its two neighbors directly instead.
mExclude :: VKey -> ((Int, VKey), (Int, VKey)) -> Input -> Input
mExclude b ((da, a), (dc, c)) =
  Map.delete b . Map.mapWithKey (\d v -> v {vNext = map (go d) (vNext v)})
  where
    go k1 (i, k2)
      | k1 == a && k2 == b = (i + dc, c)
      | k1 == c && k2 == b = (i + da, a)
      | k2 == b =
        error $
        "mExclude: found " ++
        show k1 ++
        " connected to " ++
        show k2 ++ " but it's not " ++ show a ++ " or " ++ show c
      | otherwise = (i, k2)

mExcludeOneZero :: Input -> Maybe Input
mExcludeOneZero input = do
  (b, ds) <-
    listToMaybe
      [ (b, ((da, a), (dc, c)))
      | (b, VData {vNext = [(da, a), (dc, c)], vFlow = 0}) <- Map.toList input
      , b /= vAA
      ]
  pure $ mExclude b ds input

mExcludeZero :: Input -> Input
mExcludeZero = iterateMaybeL mExcludeOneZero

mPath :: Input -> VKey -> VKey -> Int
mPath m from to =
  fst $
  lastE "mPath" $
  fromJustE "mPath" $
  aStar moves (subtract `on` fst) (const 0) ((== to) . snd) (0, from)
  where
    moves (d, k) = [(d + d', k') | (d', k') <- vNext $ mapLookupE "mPath" k m]

mValves :: Input -> [VKey]
mValves = mapFilterValues ((/= 0) . vFlow)

type Order = [VKey]

mOrders :: Input -> [Order]
mOrders = permutations . mValves

data Event =
  Event
    { eTime  :: Int
    , eValve :: VKey
    }
  deriving (Eq, Ord, Show)

mExecute :: Input -> Order -> [Event]
mExecute m = mExecuteFrom m vAA

mExecuteFrom :: Input -> VKey -> Order -> [Event]
mExecuteFrom _ _ [] = []
mExecuteFrom m a (b:bs) = ev : mExecuteFrom m b bs
  where
    ev = Event (p + 1) b
    p = mPath m a b

mTally :: Input -> Int -> [Event] -> Int
mTally m limit = go 0 0
  where
    go _ _ [] = 0
    go f t1 (Event t2 k:es)
      | t2 > limit = 0
      | otherwise = f * dt + go (f + kf) t2 es
      where
        dt = t2 - t1
        kf = vFlow $ mapLookupE "mTally" k m

mOrderScore :: Input -> Int -> Order -> Int
mOrderScore m limit o = mTally m limit $ mExecute m o

start :: Int -> Input -> Int
start minutes input0 = maximum $ map score orders
  where
    input = mExcludeZero input0
    orders = traceF (prependShow "orders length" . length) $ mOrders input
    score = mOrderScore input minutes

part1 = start 30

part2 = error "not implemented" -- start 26 2

-- on my data the right answer to part 2 is between 2722 and 2922
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651, Task part2 1707]
