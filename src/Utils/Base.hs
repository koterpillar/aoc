module Utils.Base where

import           Control.Applicative       (liftA2, (<|>))

import           Data.Bifunctor            (bimap, first, second)

import           Data.Char                 (chr, isDigit, isLower, isUpper, ord)

import           Data.Containers.ListUtils (nubInt, nubOrd)

import           Data.Either
import           Data.Either.Extra

import           Data.Foldable             (for_, toList, traverse_)

import           Data.Functor              (($>))

import           Data.Function             (on, (&))
import           Data.Function.Memoize

import           Data.List

import           Data.Map                  (Map)
import qualified Data.Map                  as Map

import           Data.Maybe

import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Data.Traversable          (for)

import           Data.Tuple

import           Lens.Micro.Platform

import           Utils.Trace

dupe :: a -> (a, a)
dupe a = (a, a)

sremove :: Int -> [a] -> (a, [a])
sremove idx lst = (lst !! idx, take idx lst ++ drop (idx + 1) lst)

sinsert :: Int -> a -> [a] -> [a]
sinsert idx val lst =
  let (hd, tl) = splitAt idx lst
   in hd ++ (val : tl)

progress :: Int -> Int -> a -> a
progress milestone amount (!val)
  | amount `mod` milestone == 0 = traceShow amount val
  | otherwise = val

listProgress :: Int -> [a] -> [a]
listProgress milestone = zipWith (progress milestone) [0 ..]

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just $ minimum xs

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs

minimumsOn :: Ord b => (a -> b) -> [a] -> [a]
minimumsOn _ [] = []
minimumsOn f as = [a | a <- as, f a == m]
  where
    m = minimum $ map f as

maximumsOn :: Ord b => (a -> b) -> [a] -> [a]
maximumsOn _ [] = []
maximumsOn f as = [a | a <- as, f a == m]
  where
    m = maximum $ map f as

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

countTrue :: [Bool] -> Int
countTrue = countIf id

inRange :: Ord k => k -> k -> k -> Bool
inRange lo hi k = lo <= k && k <= hi

zipTail :: [a] -> [(a, a)]
zipTail a = zip a (tail a)

zipWithTail :: (a -> a -> b) -> [a] -> [b]
zipWithTail f = map (uncurry f) . zipTail

zipN :: Enum a => a -> [b] -> [(a, b)]
zipN n = zip [n ..]

picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x, xs) : fmap (fmap (x :)) (picks xs)

toNothing :: Eq a => a -> a -> Maybe a
toNothing a b
  | a == b = Nothing
  | otherwise = Just b

fromJustE :: String -> Maybe a -> a
fromJustE = fromMaybe . error

fromSingleE :: Show a => String -> [a] -> a
fromSingleE _ [a]  = a
fromSingleE msg as = error $ "fromSingleE: " <> show as <> " " <> msg

headE :: String -> [a] -> a
headE msg [] = error msg
headE _ x    = head x

lastE :: String -> [a] -> a
lastE msg [] = error msg
lastE _ x    = last x

findTuple :: Foldable t => (a -> Bool) -> t (k, a) -> Maybe k
findTuple fn = fmap fst . find (fn . snd)

filterTuple :: (a -> Bool) -> [(k, a)] -> [k]
filterTuple fn = fmap fst . filter (fn . snd)
