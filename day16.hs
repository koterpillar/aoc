import Data.Char

import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

type Program = Char

type Scene = ([Int], [Program])

initial :: Int -> Scene
initial sz = unzip $ zip [0 .. sz - 1] ['a' ..]

showScene :: Scene -> String
showScene = map snd . sort . uncurry zip

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Program
            Program
  deriving (Ord, Eq, Show)

mapIs :: (Int -> Int) -> Scene -> Scene
mapIs f (is, ps) = (map f is, ps)

mapPs :: (Program -> Program) -> Scene -> Scene
mapPs f (is, ps) = (is, map f ps)

ssize :: Scene -> Int
ssize = length . fst

applyMove :: Move -> Scene -> Scene
applyMove (Spin a) scene = mapIs (\k -> (k + a) `mod` ssize scene) scene
applyMove (Exchange a b) scene = mapIs f scene
  where
    f x
      | x == a = b
      | x == b = a
      | otherwise = x
applyMove (Partner a b) scene = mapPs f scene
  where
    f x
      | x == a = b
      | x == b = a
      | otherwise = x

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
