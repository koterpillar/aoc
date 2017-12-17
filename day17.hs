#!/usr/bin/env stack
-- stack runghc
import Control.Monad

import Data.Char

import Data.IORef

import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Vector.Unboxed.Mutable as M

import System.Environment (getArgs)

import Utils

data SpinList = SpinList
  { slCurrentRef :: !(IORef Int)
  , slContents :: !(M.IOVector Int)
  }

slCurrent :: SpinList -> IO Int
slCurrent l = readIORef $ slCurrentRef l

instance Show SpinList where
  show _ = "SpinList (...)"

slPrint :: SpinList -> IO ()
slPrint l = do
  cur <- slCurrent l
  go cur 0
  putStrLn ""
  where
    go cur i = do
      showCur cur i
      putStr " "
      next <- slNext l i
      goNext cur next
    showCur cur i
      | i == cur = putStr $ "(" ++ show i ++ ")"
      | otherwise = putStr $ show i
    goNext cur 0 = pure ()
    goNext cur i = go cur i

new :: Int -> IO SpinList
new max = do
  cur <- newIORef 0
  items <- M.new max
  M.write items 0 0
  pure $ SpinList cur items

slNext :: SpinList -> Int -> IO Int
slNext (SpinList _ m) = M.read m

slNextN :: SpinList -> Int -> Int -> IO Int
slNextN sl 0 x = pure x
slNextN sl n x = slNextN sl (n - 1) x >>= slNext sl

spin :: SpinList -> Int -> Int -> IO ()
spin l amount inserted = do
  current <- slCurrent l
  newCurrent <- slNextN l amount current
  oldNext <- slNext l newCurrent
  M.write (slContents l) newCurrent inserted
  M.write (slContents l) inserted oldNext
  writeIORef (slCurrentRef l) inserted
  when (inserted `mod` 5000 == 0) $ print inserted

spins :: Int -> Int -> IO SpinList
spins amount max = do
  l <- new (max + 1)
  traverse (spin l amount) [1 .. max]
  pure l

slAfterCurrent :: SpinList -> IO Int
slAfterCurrent l = slCurrent l >>= slNext l

slAfter0 :: SpinList -> IO Int
slAfter0 l = slNext l 0

main = do
  [amount, iterations] <- map read <$> getArgs
  l <- spins amount iterations
  putStrLn "After current:"
  slAfterCurrent l >>= print
  putStrLn "After 0:"
  slAfter0 l >>= print
