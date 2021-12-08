{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text

import           Text.Parsec
import           Text.Parsec.Text

import           System.IO        (isEOF)

import           Debug.Trace

readLines :: IO [Text]
readLines =
  isEOF >>= \case
    True -> pure []
    False ->
      Text.getLine >>= \s ->
        case s of
          "" -> pure []
          _  -> fmap (s :) readLines

justParse :: Parser a -> Text -> a
justParse parser str =
  let (Right a) = parse parser "" str
   in a

readParse :: Parser a -> IO [a]
readParse parser = map (justParse parser) <$> readLines

integerP :: Parser Int
integerP = read <$> many1 digit

data Direction4
  = E
  | N
  | W
  | S
  deriving (Enum, Eq, Ord, Show)

allDir4 :: [Direction4]
allDir4 = [E, N, W, S]

turnLeft :: Direction4 -> Direction4
turnLeft S = E
turnLeft d = succ d

turnRight :: Direction4 -> Direction4
turnRight E = S
turnRight d = pred d

reverse4 :: Direction4 -> Direction4
reverse4 = turnLeft . turnLeft

data Position2 =
  Position2
    { pX :: !Int
    , pY :: !Int
    }
  deriving (Eq, Ord, Show)

walk :: Direction4 -> Position2 -> Position2
walk E (Position2 x y) = Position2 (x + 1) y
walk W (Position2 x y) = Position2 (x - 1) y
walk N (Position2 x y) = Position2 x (y - 1)
walk S (Position2 x y) = Position2 x (y + 1)

manhattanDistance :: Position2 -> Position2 -> Int
manhattanDistance (Position2 x1 y1) (Position2 x2 y2) =
  abs (x2 - x1) + abs (y2 - y1)

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

enumerate2 :: [[a]] -> [[(Position2, a)]]
enumerate2 = zipWith makeLine [0 ..]
  where
    makeLine y = zipWith (makePoint y) [0 ..]
    makePoint y x v = (Position2 x y, v)
