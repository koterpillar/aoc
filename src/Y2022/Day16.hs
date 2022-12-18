{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import           Control.Monad.State.Strict

import           Data.Foldable              (foldrM)

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Path
import           Utils

newtype VKey =
  VKey
    { unVKey :: Text
    }
  deriving (Eq, Ord)

instance Hashable VKey where
  hashWithSalt s = hashWithSalt s . unVKey

instance Memoizable VKey where
  memoize f t = memoize (f . VKey) (unVKey t)

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

type PathFn = VKey -> VKey -> Int

mPath :: Input -> PathFn
mPath m = \from to -> mapLookupE "mPath" (from, to) paths
  where
    !paths =
      traceF (prependShow "paths length" . length) $
      Map.fromList $ do
        from <- vAA : mValves m
        to <- vAA : mValves m
        guard $ from /= to
        pure ((from, to), mPath' m from to)

mPath' :: Input -> PathFn
mPath' m from to =
  fst $
  lastE "mPath" $
  fromJustE "mPath" $
  aStar moves (subtract `on` fst) (const 0) ((== to) . snd) (0, from)
  where
    moves (d, k) = [(d + d', k') | (d', k') <- vNext $ mapLookupE "mPath" k m]

mValves :: Input -> [VKey]
mValves = map fst . sortOn (negate . snd) . filter ((> 0) . snd) . Map.toList . Map.map vFlow

type Order = [VKey]

mOrders :: Input -> [Order]
mOrders = permutations . traceF (prependShow "valves" . length) . mValves

data Event =
  Event
    { eTime  :: Int
    , eValve :: VKey
    }
  deriving (Eq, Ord, Show)

mExecute :: PathFn -> Order -> [Event]
mExecute path = mExecuteFrom path vAA

mExecuteFrom :: PathFn -> VKey -> Order -> [Event]
mExecuteFrom _ _ [] = []
mExecuteFrom path a (b:bs) = ev : mExecuteFrom path b bs
  where
    ev = Event (p + 1) b
    p = path a b

mTally :: Input -> Int -> [Event] -> Int
mTally m limit = go 0 0
  where
    go f t [] = f * (limit - t)
    go f t1 (Event dt k:es)
      | t2 > limit = f * (limit - t1)
      | otherwise = f * dt + go (f + kf) t2 es
      where
        t2 = t1 + dt
        kf = vFlow $ mapLookupE "mTally" k m

start :: Int -> Input -> Int
start minutes input = maximum $ listProgress 1000000 $ take 5000000 $ map score orders
  where
    orders = mOrders input
    !path = mPath input
    score = mTally input minutes . mExecute path

part1 = start 30

part2 = error "not implemented" -- start 26 2

-- on my data the right answer to part 2 is between 2722 and 2922
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651, Task part2 1707]
