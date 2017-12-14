module Knot where

import Control.Monad

import Data.Bits

import Data.Char

import Data.List
import Data.List.Split

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Numeric

import Utils

data Knot = Knot
  { kItems :: !(Map Int Int) -- ^ map positions to numbers
  , kPos :: !Int
  , kSkipSize :: !Int
  }

kAt :: Knot -> Int -> Int
kAt k i =
  let (Just v) = Map.lookup i (kItems k)
  in v

kLength = Map.size . kItems

mkKnot :: Int -> Knot
mkKnot len =
  Knot
  { kItems = Map.fromList $ zip [0 .. len - 1] [0 .. len - 1]
  , kPos = 0
  , kSkipSize = 0
  }

instance Show Knot where
  show k =
    "Knot {" ++
    unwords (map showKItem [0 .. kLength k - 1]) ++
    ", kSkipSize = " ++ show (kSkipSize k) ++ "}"
    where
      showKItem i
        | i == kPos k = "[" ++ show (kAt k i) ++ "]"
        | otherwise = show (kAt k i)

kNorm :: Knot -> Int -> Int
kNorm knot a = a `mod` kLength knot

kAdd :: Knot -> Int -> Int -> Int
kAdd knot a b = kNorm knot $ a + b

kSub :: Knot -> Int -> Int -> Int
kSub knot a b = kNorm knot $ a - b

kSkip :: Int -> Knot -> Knot
kSkip size knot = knot {kPos = kAdd knot (kPos knot) size}

kIncSkip :: Knot -> Knot
kIncSkip k = k {kSkipSize = kSkipSize k + 1}

kRotate :: Int -> Knot -> Knot
kRotate len knot = knot {kItems = Map.mapKeys rotKey $ kItems knot}
  where
    rotKey i
      | i - pos < len = pos + pos + len - i - 1
      | otherwise = i
    pos = kPos knot
    (+) = kAdd knot
    (-) = kSub knot

kStep :: Int -> Knot -> Knot
kStep len knot = kIncSkip $ kSkip (kSkipSize knot + len) $ kRotate len knot
  where
    (+) = kAdd knot

kSteps :: [Int] -> Knot -> Knot
kSteps lens knot = foldr kStep knot $ reverse lens

strToLens :: String -> [Int]
strToLens str = join $ replicate 64 $ map ord str ++ [17, 31, 73, 47, 23]

kHash :: String -> Knot -> Knot
kHash str = kSteps $ strToLens str

kDense :: Knot -> [Int]
kDense = map (foldr1 xor) . chunksOf 16 . Map.elems . kItems

kShowDense :: [Int] -> String
kShowDense = join . map (pad' '0' 2 . flip showHex "")

knotHash :: String -> String
knotHash str = kShowDense $ kDense $ kHash str $ mkKnot 256
