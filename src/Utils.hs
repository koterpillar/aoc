module Utils
  ( module Utils
  , module Data.Containers.ListUtils
  , module Data.Either
  , module Data.Maybe
  , module Data.List
  , module Data.Tuple
  , module Utils.Trace
  , Map
  , Set
  , Text
  , ($>)
  , (&)
  , (<=<)
  , (<|>)
  , (>=>)
  , bimap
  , chr
  , guard
  , first
  , forM_
  , isDigit
  , isLower
  , isUpper
  , join
  , liftA2
  , on
  , ord
  , readEither
  , second
  , splitOn
  , traverse_
  , unless
  , when
  ) where

import           Control.Applicative       (liftA2, (<|>))
import           Control.Monad

import           Data.Bifunctor            (bimap, first, second)

import           Data.Char                 (chr, isDigit, isLower, isUpper, ord)

import           Data.Containers.ListUtils (nubInt, nubOrd)

import           Data.Either

import           Data.Foldable             (traverse_)

import           Data.Functor              (($>))

import           Data.Function             (on, (&))

import           Data.List
import           Data.List.Split           (splitOn)

import           Data.Map                  (Map)
import qualified Data.Map                  as Map

import           Data.Maybe

import           Data.Set                  (Set)

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Data.Tuple

import           Text.Read

import           Utils.Trace

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ v = v
iterateN n fn v =
  let !u = iterateN (n - 1) fn v
   in progress 1000 n $ fn u

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile continue fn v
  | continue v =
    let v' = fn v
     in v : iterateWhile continue fn v'
  | otherwise = []

iterateSettle :: Eq a => (a -> a) -> a -> a
iterateSettle fn v =
  if v == v'
    then v
    else iterateSettle fn v'
  where
    v' = fn v

dupe :: a -> (a, a)
dupe a = (a, a)

pad :: Int -> Text -> Text
pad sz = Text.justifyRight sz ' '

sset :: Int -> a -> [a] -> [a]
sset idx val lst = take idx lst ++ [val] ++ drop (idx + 1) lst

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

mapFromListSum :: (Ord k, Num a) => [(k, a)] -> Map k a
mapFromListSum = Map.fromListWith (+)

mapFromListCount :: (Ord k, Num a) => [k] -> Map k a
mapFromListCount = mapFromListSum . map (, 1)

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

countTrue :: [Bool] -> Int
countTrue = countIf id

ttrim :: Text -> Text
ttrim = Text.dropWhile (== '\n') . Text.dropWhileEnd (== '\n')

tshow :: Show a => a -> Text
tshow = Text.pack . show

zipTail :: [a] -> [(a, a)]
zipTail a = zip a (tail a)

zipWithTail :: (a -> a -> b) -> [a] -> [b]
zipWithTail f = map (uncurry f) . zipTail

mostCommon :: Ord a => [a] -> Maybe a
mostCommon = fmap snd . maybeMaximum . map swap . Map.toList . mapFromListCount

fromJustE :: String -> Maybe a -> a
fromJustE = fromMaybe . error

boundedAll :: (Bounded a, Enum a) => [a]
boundedAll = [minBound .. maxBound]
