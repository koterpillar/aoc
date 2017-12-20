{-# LANGUAGE DeriveFunctor #-}

import Data.List

import Data.Either

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ord

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.Number
import Text.Parsec.String

import Utils

data R3 = R3
  { r3x :: !Int
  , r3y :: !Int
  , r3z :: !Int
  } deriving (Ord, Eq, Show)

data Pt v = Pt
  { ppos :: !v
  , pvel :: !v
  , pacc :: !v
  } deriving (Eq, Show, Functor)

type P3 = Pt R3

parsep3 :: Parser P3
parsep3 =
  Pt <$> (string "p=" *> parser3) <*> (string ", v=" *> parser3) <*>
  (string ", a=" *> parser3)

parser3 :: Parser R3
parser3 =
  string "<" *> (R3 <$> num <*> (string "," *> num) <*> (string "," *> num)) <*
  string ">"
  where
    num = many (char ' ') *> int

distanceToZero :: R3 -> Int
distanceToZero (R3 x y z) = x + y + z

-- | Align the second point with the directions of the first
--
-- > realign (R3 -2 -3 2) (R1 1 -1 1) = (-1 1 1)
realign :: R3 -> R3 -> R3
realign (R3 sx sy sz) (R3 x y z) = R3 (r sx x) (r sy y) (r sz z)
  where
    r s v
      | s < 0 = negate v
      | otherwise = v

closerInTheLongRun :: P3 -> P3 -> Ordering
closerInTheLongRun = comparing closeness

closeness (Pt p v a) = (distanceToZero a', distanceToZero v', distanceToZero p')
  where
    a' = realign a a
    v' = realign a v
    p' = realign a p

closestInTheLongRun :: [P3] -> (Int, P3)
closestInTheLongRun pts = (closestIndex, closestPt)
  where
    closest = minimumBy closerInTheLongRun pts
    Just closestIndex = elemIndex closest pts
    closestPt = pts !! closestIndex
