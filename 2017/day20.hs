{-# LANGUAGE DeriveFunctor #-}

import           Data.List

import           Data.Either

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.Ord

import           Data.Maybe

import           Data.Set           (Set)
import qualified Data.Set           as Set

import           Text.Parsec
import           Text.Parsec.Number
import           Text.Parsec.String

import           Utils

data R3 =
  R3
    { r3x :: !Int
    , r3y :: !Int
    , r3z :: !Int
    }
  deriving (Ord, Eq, Show)

r3add :: R3 -> R3 -> R3
r3add (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 + x2) (y1 + y2) (z1 + z2)

r3sub :: R3 -> R3 -> R3
r3sub (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 - x2) (y1 - y2) (z1 - z2)

data Pt v =
  Pt
    { ppos :: !v
    , pvel :: !v
    , pacc :: !v
    }
  deriving (Eq, Show, Functor)

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
-- > realign3 (R3 -2 -3 2) (R1 1 -1 1) = (-1 1 1)
realign3 :: R3 -> R3 -> R3
realign3 (R3 sx sy sz) (R3 x y z) =
  R3 (realign sx x) (realign sy y) (realign sz z)

realign s v
  | s < 0 = negate v
  | otherwise = v

closerInTheLongRun :: P3 -> P3 -> Ordering
closerInTheLongRun = comparing closeness

closeness (Pt p v a) = (distanceToZero a', distanceToZero v', distanceToZero p')
  where
    a' = realign3 a a
    nextalign =
      if a == R3 0 0 0
        then v
        else a
    v' = realign3 nextalign v
    p' = realign3 nextalign p

closestInTheLongRun :: [P3] -> (Int, P3)
closestInTheLongRun pts = (closestIndex, closestPt)
  where
    closest = minimumBy closerInTheLongRun pts
    Just closestIndex = elemIndex closest pts
    closestPt = pts !! closestIndex

step :: P3 -> P3
step (Pt p v a) = Pt p' v' a
  where
    v' = v `r3add` a
    p' = p `r3add` v'

p3sub :: P3 -> P3 -> P3
p3sub (Pt p1 v1 a1) (Pt p2 v2 a2) = Pt (r3sub p1 p2) (r3sub v1 v2) (r3sub a1 a2)

receding :: P3 -> P3 -> Bool
receding p1 p2 = receding' $ closeness $ p3sub p1 p2
  where
    receding' (da, dv, dp) =
      let vals = [da, dv, dp]
       in all (>= 0) vals && any (> 0) vals

collide :: Maybe Int -> P3 -> P3 -> Maybe Int
collide maxT p1 p2 = go p1 p2 0
  where
    go p1 p2 t
      | maybe False (t >) maxT = Nothing
      | ppos p1 == ppos p2 = Just t
      | receding p1 p2 = Nothing
      | otherwise = go (step p1) (step p2) (t + 1)

collide' :: P3 -> P3 -> Maybe Int
collide' = collide Nothing

survivors :: [P3] -> [P3]
survivors pts = map fst $ filter (not . dies) pts'
  where
    lifetime pt = maybeMinimum $ mapMaybe (collide' pt) $ filter (pt /=) pts
    pts' = map (\pt -> (pt, lifetime pt)) pts
    collideL _ (_, Nothing) = False
    collideL (_, Nothing) _ = False
    collideL (pt1, Just l1) (pt2, Just l2) =
      let lmin = min l1 l2
       in pt1 /= pt2 && isJust (collide (Just lmin) pt1 pt2)
    dies ptl = any (collideL ptl) pts'
