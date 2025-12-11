{-# LANGUAGE Strict #-}

module Y2022.Day16
  ( tasks
  ) where

import           Control.Monad.State.Strict

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Path
import           Utils

newtype VKey = VKey
  { unVKey :: Text
  } deriving (Eq, Ord, Generic)

instance Hashable VKey

vAA :: VKey
vAA = VKey "AA"

instance Show VKey where
  show (VKey k) = Text.unpack k

data VData = VData
  { vFlow :: Int
  , vNext :: [(Int, VKey)]
  } deriving (Eq, Ord, Show)

mkVData :: Int -> [VKey] -> VData
mkVData f n = VData f $ zip (repeat 1) n

type Input = Map VKey VData

iValves :: Input -> [VKey]
iValves = mapFilterValues $ (> 0) . vFlow

parser :: Parser Text Input
parser =
  Map.fromList
    <$> linesP &** wordsP &* pureP tail &* unconsP &* (pureP VKey &= vdp)
  where
    vdp =
      pureP (drop 2)
        &* (uncurry mkVData
              <$> unconsP
                    &* ((pureP (terase "rate=" . terase ";") &* integerP)
                          &= (pureP (drop 4 . map (terase ",")) &** pureP VKey)))

type PathFn = VKey -> VKey -> Int

mPath :: Input -> PathFn
mPath m = \from to -> mapLookupE "mPath" (from, to) paths
  where
    !paths =
      Map.fromList $ do
        from <- vAA : mValves m
        to <- vAA : mValves m
        guard $ from /= to
        pure ((from, to), mPath' m from to)

mPath' :: Input -> PathFn
mPath' m from to =
  fst
    $ lastE "mPath"
    $ fromJustE "mPath"
    $ aStar moves (subtract `on` fst) (const 0) ((== to) . snd) (0, from)
  where
    moves (d, k) = [(d + d', k') | (d', k') <- vNext $ mapLookupE "mPath" k m]

mValves :: Input -> [VKey]
mValves =
  map fst
    . sortOn (negate . snd)
    . filter ((> 0) . snd)
    . Map.toList
    . Map.map vFlow

mTally :: Input -> PathFn -> Int -> [VKey] -> Int
mTally m path limit = go 0 0 vAA
  where
    go f t _ [] = f * (limit - t)
    go f1 t1 p1 vs
      | t1 > limit = f1 * (limit - t1)
      | otherwise =
        maximum $ do
          (p2, vrest) <- picks vs
          let dt = path p1 p2 + 1
          let df = vFlow $ mapLookupE "mTally" p2 m
          let t2 = t1 + dt
          pure
            $ if t2 > limit
                then f1 * (limit - t1)
                else let f2 = f1 + df
                      in f1 * dt + go f2 t2 p2 vrest

start :: Int -> Input -> Int
start minutes input = score
  where
    !path = mPath input
    score =
      mTally input path minutes
        $ traceF (prependShow "valves" . length)
        $ mValves input

part1 = start 30

splits :: Ord a => [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = do
  (ls, rs) <- splits xs
  [(x : ls, rs), (ls, x : rs)]

start2 :: Int -> Input -> Int
start2 minutes input =
  maximum
    $ map (uncurry (+) . (score *** score))
    $ listProgress 100
    $ traceF (prependShow "splits" . length)
    $ splits
    $ mValves input
  where
    !path = mPath input
    score = mTally input path minutes

-- Takes around 5 minutes
part2 = start2 26

tasks = Tasks (AOC 2022 16) (CodeBlock 0) parser [Task part1 1651, Task part2 1707]
