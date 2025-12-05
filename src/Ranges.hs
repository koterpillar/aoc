module Ranges where

import Data.List (uncons)
import Data.List.Extra (unsnoc)

-- Ranges are a list of non-overlapping half-open intervals [a, b)
newtype Ranges = Ranges
    { toList :: [(Int, Int)]
    }
    deriving (Ord, Eq, Show)

empty :: Ranges
empty = Ranges []

_mk1 :: Int -> Int -> (Int, Int)
_mk1 a b
    | a == b = error $ "singleton: empty interval " <> show (a, b)
    | a > b = error $ "singleton: negative interval " <> show (a, b)
    | otherwise = (a, b)

unsafeFromList :: [(Int, Int)] -> Ranges
unsafeFromList = Ranges . map (uncurry _mk1)

singleton :: Int -> Int -> Ranges
singleton a b = Ranges [_mk1 a b]

_apply :: (Int -> Int -> Ranges -> Ranges) -> Ranges -> Ranges -> Ranges
_apply f y x = foldr (uncurry f) x $ toList y

_add1 :: Int -> Int -> Ranges -> Ranges
_add1 a b = unsafeFromList . go a b . toList
  where
    go a b [] = [(a, b)]
    go a b ((c, d) : rs)
        | b < c = (a, b) : (c, d) : rs
        | d < a = (c, d) : go a b rs
        | otherwise = go (min a c) (max b d) rs

union :: Ranges -> Ranges -> Ranges
union = _apply _add1

fromList :: [(Int, Int)] -> Ranges
fromList = foldr union empty . map (uncurry singleton)

_remove1 :: Int -> Int -> Ranges -> Ranges
_remove1 a b = unsafeFromList . go . toList
  where
    go [] = []
    go ((c, d) : rs)
        | b <= c = (c, d) : rs
        | d <= a = (c, d) : go rs
        | a <= c && d <= b = go rs
        | a <= c = (b, d) : go rs
        | d <= b = (c, a) : go rs
        | otherwise = (c, a) : (b, d) : go rs

subtract :: Ranges -> Ranges -> Ranges
subtract = _apply _remove1

minimum :: Ranges -> Maybe Int
minimum = fmap (fst . fst) . uncons . toList

maximum :: Ranges -> Maybe Int
maximum = fmap (snd . snd) . unsnoc . toList

length :: Ranges -> Int
length = sum . map (uncurry Prelude.subtract) . toList

lengthInclusive :: Ranges -> Int
lengthInclusive = sum . map (succ . uncurry Prelude.subtract) . toList

member :: Int -> Ranges -> Bool
member x (Ranges rs) = any inRange rs
  where
    inRange (a, b) = a <= x && x < b
