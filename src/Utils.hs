module Utils
  ( module Utils
  , module Data.Maybe
  , module Data.List
  , ($>)
  , join
  , splitOn
  , traceShow
  , traceShowId
  , traceShowM
  , traverse_
  ) where

import           Control.Monad    (join, when)

import           Data.Char        (ord)

import           Data.Foldable    (traverse_)

import           Data.Functor     (($>))

import           Data.List
import           Data.List.Split  (splitOn)

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Data.Maybe

import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text

import           Text.Parsec
import           Text.Parsec.Text

import           Debug.Trace

parseLines :: Parser a -> Text -> [a]
parseLines parser = map (justParse parser) . Text.lines

justParse :: Parser a -> Text -> a
justParse parser str =
  case parse parser "" str of
    Right a -> a
    Left e  -> error $ show e

digitP :: Parser Int
digitP = (\c -> ord c - ord '0') <$> digit

digitsP :: Parser [Int]
digitsP = many1 digitP

integerP :: Parser Int
integerP = read <$> many1 digit

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

mapByIndex :: [Int] -> Map Int Int
mapByIndex = foldr (\timer -> Map.insertWith (+) timer 1) Map.empty

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

countTrue :: [Bool] -> Int
countTrue = countIf id

tshow :: Show a => a -> Text
tshow = Text.pack . show

tread :: Read a => Text -> a
tread = read . Text.unpack

ttrace :: Text -> a -> a
ttrace = trace . Text.unpack

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual
  | expected == actual = pure ()
  | otherwise =
    error $
    message <> " is " <> show actual <> ", but expected " <> show expected
