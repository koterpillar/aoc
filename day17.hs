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

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import System.Environment (getArgs)

import Utils

data SpinList = SpinList
  { slCurrentRef :: !(IORef Int)
  , slContents :: !(M.IOVector Int)
  , slLengthRef :: !(IORef Int)
  , slSkipSize :: !Int
  , slSkips :: !(M.IOVector Int)
  }

slCurrent :: SpinList -> IO Int
slCurrent = readIORef . slCurrentRef

slLength :: SpinList -> IO Int
slLength = readIORef . slLengthRef

instance Show SpinList where
  show _ = "SpinList (...)"

slPrint :: SpinList -> IO ()
slPrint l = do
  cur <- slCurrent l
  go cur 0
  putStrLn ""
  putStrLn $ "skip size = " ++ show (slSkipSize l)
  U.freeze (slSkips l) >>= print
  print $ M.length $ slContents l
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

new :: Int -> Int -> IO SpinList
new max skipSize = do
  cur <- newIORef 0
  len <- newIORef 1
  items <- M.new max
  M.write items 0 0
  skips <- M.new max
  M.write skips 0 0
  pure
    SpinList
    { slCurrentRef = cur
    , slLengthRef = len
    , slContents = items
    , slSkipSize = skipSize
    , slSkips = skips
    }

slNext :: SpinList -> Int -> IO Int
slNext l = M.read (slContents l)

slNextNSlow :: SpinList -> Int -> Int -> IO Int
slNextNSlow sl 0 x = pure x
slNextNSlow sl n x
  | n < 0 = error "slNextNSlow: n < 0"
  | otherwise = slNextNSlow sl (n - 1) x >>= slNext sl

slNextN :: SpinList -> Int -> Int -> IO Int
slNextN sl n x
  | n < slSkipSize sl = slNextNSlow sl n x
  | n < 0 = error "slNextN: n < 0"
  | otherwise = do
    x' <- M.read (slSkips sl) x
    slNextN sl (n - slSkipSize sl) x'

slSet :: SpinList -> Int -> Int -> IO ()
slSet l = M.write (slContents l)

slSetSkip :: SpinList -> Int -> Int -> IO ()
slSetSkip l = M.write (slSkips l)

slSetSkips :: SpinList -> Int -> Int -> Int -> IO ()
slSetSkips l _ _ 0 = pure ()
slSetSkips l source target cnt = do
  targetCheck <- slNextNSlow l (slSkipSize l) source
  -- when (target /= targetCheck) $ do
  --   slPrint l
  --   print $ "Setting skip from " ++ show source ++ " to " ++ show target
  --   print $ "...but should be " ++ show targetCheck
  --   error "wrong skip target"
  slSetSkip l source target
  source' <- slNext l source
  target' <- slNext l target
  slSetSkips l source' target' (cnt - 1)

slResetSkips :: SpinList -> IO ()
slResetSkips l = do
  let source = 0
  target <- slNextNSlow l (slSkipSize l) source
  len <- slLength l
  slSetSkips l source target len

spin :: SpinList -> Int -> Int -> IO ()
spin l amount inserted = do
  let skipSize = slSkipSize l
  current <- slCurrent l
  brokenSkipsStart <- slNextN l (amount - skipSize + 1) current
  newCurrent <- slNextN l (skipSize - 1) brokenSkipsStart
  oldNext <- slNext l newCurrent
  slSet l newCurrent inserted
  slSet l inserted oldNext
  writeIORef (slCurrentRef l) inserted
  if inserted <= slSkipSize l
    then slResetSkips l
    else slSetSkips l brokenSkipsStart inserted (skipSize + 1)
  when (inserted `mod` 5000 == 0) $ print inserted

spins :: Int -> Int -> IO SpinList
spins amount max = do
  l <- new (max + 1) (optimalSkip amount)
  print $ "Optimal skip = " ++ show (slSkipSize l)
  traverse (spin l amount) [1 .. max]
  pure l

optimalSkip :: Int -> Int
optimalSkip a = ceiling $ (fromIntegral a / 2) ** (1 / 3)

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
