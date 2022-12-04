#!/usr/bin/env stack
-- stack runghc
import           Control.Applicative
import           Control.Monad

import           Data.Char

import           Data.List
import           Data.List.Split
import           Data.List.Utils

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.Maybe

import           Data.Monoid

import           Data.Set            (Set)
import qualified Data.Set            as Set

import           System.Environment  (getArgs)

import           Utils

data Tree a
  = Tree
      { tSizeCache :: !Int
      , tLeft      :: !(Tree a)
      , tRight     :: !(Tree a)
      }
  | Leaf
      { tValue :: !a
      }
  deriving (Show)

tSize :: Tree a -> Int
tSize t@Tree {} = tSizeCache t
tSize t@Leaf {} = 1

tAt :: Int -> Tree a -> a
tAt 0 (Leaf v) = v
tAt e (Leaf v) = error $ "Cannot look up " ++ show e ++ " in a leaf"
tAt n (Tree _ l r)
  | n < tSize l = tAt n l
  | otherwise = tAt (n - tSize l) r

tInsertAfter :: Int -> a -> Tree a -> Tree a
tInsertAfter 0 v t@(Leaf _) = Tree 2 t (Leaf v)
tInsertAfter e v t@(Leaf _) = error $ "Cannot insert into leaf at " ++ show e
tInsertAfter n v t@Tree {}
  | n < tSize (tLeft t) =
    Tree (tSize t + 1) (tInsertAfter n v (tLeft t)) (tRight t)
  | otherwise =
    Tree
      (tSize t + 1)
      (tLeft t)
      (tInsertAfter (n - tSize (tLeft t)) v (tRight t))

tFlatten :: Tree a -> [a]
tFlatten (Leaf v)     = [v]
tFlatten (Tree _ l r) = tFlatten l <> tFlatten r

tIndexOf :: Eq a => a -> Tree a -> Maybe Int
tIndexOf a (Leaf b)
  | a == b = Just 0
  | otherwise = Nothing
tIndexOf a t@Tree {} = i1 <|> i2
  where
    i1 = tIndexOf a (tLeft t)
    i2 = (+ tSize (tLeft t)) <$> tIndexOf a (tRight t)

data SpinList =
  SpinList
    { slCurrent  :: !Int
    , slContents :: !(Tree Int)
    }

slLength :: SpinList -> Int
slLength = tSize . slContents

instance Show SpinList where
  show l = unwords $ zipWith showCurrent (tFlatten (slContents l)) [0 ..]
    where
      showCurrent n i
        | i == slCurrent l = "(" ++ show n ++ ")"
        | otherwise = show n

new :: SpinList
new = SpinList {slCurrent = 0, slContents = Leaf 0}

spin :: Int -> SpinList -> Int -> SpinList
spin amount l inserted =
  progress 10000 inserted SpinList {slCurrent = cur' + 1, slContents = t'}
  where
    cur = slCurrent l
    cur' = slAdd l cur amount
    t = slContents l
    t' = tInsertAfter cur' inserted t

spins :: Int -> Int -> SpinList
spins amount max = foldl' (spin amount) new [1 .. max]

slAdd :: SpinList -> Int -> Int -> Int
slAdd sl i j = (i + j) `mod` slLength sl

slInc :: SpinList -> Int -> Int
slInc sl = slAdd sl 1

slAt :: Int -> SpinList -> Int
slAt i sl = tAt i (slContents sl)

slAfterCurrent :: SpinList -> Int
slAfterCurrent l = slAt (slInc l (slCurrent l)) l

slAfter0 :: SpinList -> Int
slAfter0 l = slAt iafter0 l
  where
    root = slContents l
    (Just i0) = tIndexOf 0 root
    iafter0 = slInc l i0

main = do
  [amount, iterations] <- map read <$> getArgs
  let l = spins amount iterations
  putStrLn "After current:"
  print $ slAfterCurrent l
  putStrLn "After 0:"
  print $ slAfter0 l
