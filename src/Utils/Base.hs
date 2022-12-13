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
import           Data.List.Extra

import           Data.Map                  (Map)
import qualified Data.Map                  as Map

import           Data.Maybe

import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Data.Traversable          (for)

import           Data.Tuple

import           Text.Read

import           Utils.Trace

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ v = v
iterateN n fn v =
  let !u = iterateN (n - 1) fn v
   in progress 1000000 n $ fn u

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

iterateEither :: (a -> Either b a) -> a -> b
iterateEither fn v =
  case fn v of
    Left b  -> b
    Right v -> iterateEither fn v

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

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

countTrue :: [Bool] -> Int
countTrue = countIf id

inRange :: Ord k => k -> k -> k -> Bool
inRange lo hi k = lo <= k && k <= hi

ttrim :: Text -> Text
ttrim = Text.dropWhile (== '\n') . Text.dropWhileEnd (== '\n')

tshow :: Show a => a -> Text
tshow = Text.pack . show

treplace :: Text -> Text -> Text -> Text
treplace = Text.replace

terase :: Text -> Text -> Text
terase piece = Text.replace piece ""

tlength :: Text -> Int
tlength = Text.length

zipTail :: [a] -> [(a, a)]
zipTail a = zip a (tail a)

zipWithTail :: (a -> a -> b) -> [a] -> [b]
zipWithTail f = map (uncurry f) . zipTail

zipN :: Enum a => a -> [b] -> [(a, b)]
zipN n = zip [n ..]

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

instance Memoizable Text where
  memoize f t = memoize (f . Text.pack) (Text.unpack t)

findTuple :: Foldable t => (a -> Bool) -> t (k, a) -> Maybe k
findTuple fn = fmap fst . find (fn . snd)

filterTuple :: (a -> Bool) -> [(k, a)] -> [k]
filterTuple fn = fmap fst . filter (fn . snd)
