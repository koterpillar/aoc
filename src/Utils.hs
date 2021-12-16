module Utils
  ( module Utils
  , module Data.Maybe
  , module Data.List
  , Map
  , Set
  , Text
  , ($>)
  , (<=<)
  , (>=>)
  , chr
  , isDigit
  , isLower
  , isUpper
  , join
  , liftA2
  , on
  , ord
  , splitOn
  , swap
  , traceShow
  , traceShowId
  , traceShowM
  , traverse_
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad       (join, when, (<=<), (>=>))

import           Data.Char           (chr, isDigit, isLower, isUpper, ord)

import           Data.Foldable       (traverse_)

import           Data.Functor        (($>))

import           Data.Function       (on)

import           Data.List
import           Data.List.Split     (splitOn)

import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Data.Maybe

import           Data.Set            (Set)

import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text

import           Data.Tuple          (swap)

import           Debug.Trace

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

tread :: Read a => Text -> a
tread = read . Text.unpack

ttrace :: Text -> a -> a
ttrace = trace . Text.unpack

ttraceF :: (a -> Text) -> a -> a
ttraceF f a = ttrace (f a) a

traceShowF :: Show b => (a -> b) -> a -> a
traceShowF f a = traceShow (f a) a

zipTail :: [a] -> [(a, a)]
zipTail a = zip a (tail a)

zipWithTail :: (a -> a -> b) -> [a] -> [b]
zipWithTail f = map (uncurry f) . zipTail

mostCommon :: Ord a => [a] -> Maybe a
mostCommon = fmap snd . maybeMaximum . map swap . Map.toList . mapFromListCount

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual
  | expected == actual = pure ()
  | otherwise =
    error $
    message <> " is " <> show actual <> ", but expected " <> show expected
