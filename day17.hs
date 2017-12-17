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

data SpinList = SpinList
  { slPos :: !Int
  , slLength :: !Int
  , slItems :: ![Int]
  } deriving (Ord, Eq, Show)

new :: SpinList
new = SpinList 0 1 [0]

spin :: Int -> Int -> SpinList -> SpinList
spin amount inserted l = SpinList newPos (slLength l + 1) newItems
  where
    newPos = (slPos l + amount) `mod` slLength l + 1
    newItems = sinsert newPos inserted (slItems l)

spins :: Int -> Int -> SpinList
spins amount max = foldl' (flip (spin amount)) new [1..max]

slSample :: SpinList -> [Int]
slSample l = take 5 $ drop (slPos l) (slItems l)
