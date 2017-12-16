import Data.Char

import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

type Program = Char

idmap :: Ord a => [a] -> Map a a
idmap els = Map.fromList $ zip els els

newtype Scene =
  Scene (Map Int Program)

initial :: Int -> Scene
initial sz = Scene $ Map.fromList $ zip [0 .. sz - 1] (take sz ['a' ..])

instance Show Scene where
  show (Scene m) = Map.elems m

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Program
            Program
  deriving (Ord, Eq, Show)

ssize :: Scene -> Int
ssize (Scene m) = Map.size m

swap2 :: Eq a => a -> a -> a -> a
swap2 a b x
  | x == a = b
  | x == b = a
  | otherwise = x

applyMove :: Move -> Scene -> Scene
applyMove (Spin a) scene@(Scene m) =
  Scene $ Map.mapKeys (\k -> (k + a) `mod` ssize scene) m
applyMove (Exchange a b) (Scene m) = Scene $ Map.mapKeys (swap2 a b) m
applyMove (Partner a b) (Scene m) = Scene $ Map.map (swap2 a b) m

commands :: String -> [Move]
commands = map parseMove . splitOn ","
  where
    parseMove ('s':x) = Spin $ read x
    parseMove ('x':rest) =
      let [a, b] = splitOn "/" rest
      in Exchange (read a) (read b)
    parseMove ['p', a, '/', b] = Partner a b

fmove :: [Move] -> Scene -> Scene
fmove moves scene = foldl' (flip applyMove) scene moves

example :: [Move]
example = commands "s1,x3/4,pe/b"

data Moves =
  Moves !(Map Int Int)
        !(Map Program Program)
  deriving (Show)

iempty :: Int -> Map Int Int
iempty sz = idmap [0..sz - 1]

pempty :: Int -> Map Program Program
pempty sz = idmap $ take sz ['a'..]

emptyMoves :: Int -> Moves
emptyMoves sz = Moves (iempty sz) (pempty sz)

mmult :: (Ord a, Ord b) => Map a b -> Map b c -> Map a c
mmult m1 m2 = Map.map (\v -> fromJust $ Map.lookup v m2) m1

toMoves :: Int -> [Move] -> Moves
toMoves sz = go $ emptyMoves sz
  where
    go mm [] = mm
    go (Moves im pm) (Spin a:rs) = go (Moves (spin `mmult` im) pm) rs
      where
        spin = Map.fromList [(i, (i - a) `mod` sz) | i <- [0 .. sz - 1]]
    go (Moves im pm) (Exchange a b:rs) = go (Moves (iswap sz a b `mmult` im) pm) rs
    go (Moves im pm) (Partner a b:rs) = go (Moves im (pm `mmult` pswap sz a b)) rs

iswap :: Int -> Int -> Int -> Map Int Int
iswap sz a b = Map.insert a b $ Map.insert b a $ iempty sz

pswap :: Int -> Program -> Program -> Map Program Program
pswap sz a b = Map.insert a b $ Map.insert b a $ pempty sz

applyMoves :: Moves -> Scene -> Scene
applyMoves (Moves im pm) (Scene m) = Scene $ im `mmult` m `mmult` pm
