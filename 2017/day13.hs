import           Data.List.Utils

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Maybe

import           Data.Monoid

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Utils

type Firewall = Map Int Int -- depth to range

example :: Firewall
example = Map.fromList [(0, 3), (1, 2), (4, 4), (6, 4)]

readFirewall :: IO Firewall
readFirewall = Map.fromList . map (parse . words . replace ":" "") <$> readLines
  where
    parse [ds, rs] = (read ds, read rs)

fPosition :: Firewall -> Int -> Int -> Maybe Int
fPosition fw time depth =
  case Map.lookup depth fw of
    Nothing    -> Nothing
    Just range -> Just $ agentPosition range time

agentPosition :: Int -> Int -> Int
agentPosition range time =
  let loop = (range - 1) * 2
      m = time `mod` loop
   in if m >= range
        then loop - m
        else m

fMaxDepth :: Firewall -> Int
fMaxDepth = maximum . Map.keys

fSeverity :: Firewall -> Int -> Int
fSeverity fw depth =
  case Map.lookup depth fw of
    Just range -> depth * range
    _          -> 0

fTotalSeverity :: Int -> Firewall -> Int
fTotalSeverity delay fw =
  sum $ zipWith sev [0 .. fMaxDepth fw] [delay .. delay + fMaxDepth fw]
  where
    sev depth time =
      case fPosition fw time depth of
        (Just 0) -> fSeverity fw depth
        _        -> 0

fCaught :: Int -> Firewall -> Bool
fCaught delay fw =
  or $ zipWith caught [0 .. fMaxDepth fw] [delay .. delay + fMaxDepth fw]
  where
    caught depth time =
      case fPosition fw time depth of
        (Just 0) -> True
        _        -> False
