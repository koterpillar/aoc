module Y2023.Day24 where

import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as Text

import           Data.Tuple.Extra

import           Data.Ratio

import           AOC
import           Utils

type RInt = Ratio Integer

type P3 = (RInt, RInt, RInt)

data Hailstone t = Hailstone
  { hPos :: t
  , hVel :: t
  } deriving (Eq, Ord, Show)

instance Functor Hailstone where
  fmap f (Hailstone p v) = Hailstone (f p) (f v)

class Manifold t c where
  tMulPlus :: t -> c -> c -> c

instance Manifold RInt RInt where
  tMulPlus t v x = t * v + x

instance (Manifold a b, Manifold a c) => Manifold a (b, c) where
  tMulPlus t (v1, v2) (x1, x2) = (tMulPlus t v1 x1, tMulPlus t v2 x2)

hAt :: Manifold t c => t -> Hailstone c -> c
hAt t (Hailstone p v) = tMulPlus t v p

parser :: Parser Text [Hailstone P3]
parser = linesP &** tsplitP " @ " &* ap2P Hailstone p3p p3p
  where
    p3p = tsplitP "," &* ap3P (,,) ratioP ratioP ratioP
    ratioP = fromIntegral <$> readP @Integer

type Range = (RInt, RInt)

exampleRange :: Range
exampleRange = (7, 27)

taskRange :: Range
taskRange = (200000000000000, 400000000000000)

pairs :: [a] -> [(a, a)]
pairs []    = []
pairs [_]   = []
pairs (x:r) = map (x, ) r ++ pairs r

maybeDiv :: (Fractional a, Eq a) => a -> a -> Maybe a
maybeDiv _ 0 = Nothing
maybeDiv x y = Just $ x / y

hIntersectXY ::
     (Fractional a, Eq a, Manifold a a, Show a)
  => Hailstone (a, a)
  -> Hailstone (a, a)
  -> Maybe (a, a, (a, a))
hIntersectXY hA@(Hailstone (xA, yA) (vxA, vyA)) hB@(Hailstone (xB, yB) (vxB, vyB)) = do
  tA <-
    (vyB * xB - vxB * yB - vyB * xA + vxB * yA)
      `maybeDiv` (vyB * vxA - vxB * vyA)
  tB <-
    (vyA * xA - vxA * yA - vyA * xB + vxA * yB)
      `maybeDiv` (vyA * vxB - vxA * vyB)
  let rA = hAt tA hA
  let rB = hAt tB hB
  if rA /= rB
    then error $ "hIntersectXY: " <> show rA <> " /= " <> show rB
    else pure (tA, tB, rA)

hXY :: Hailstone (a, b, c) -> Hailstone (a, b)
hXY = fmap (\(x, y, _) -> (x, y))

intersectXYWithin :: Range -> Hailstone P3 -> Hailstone P3 -> Bool
intersectXYWithin r h1 h2 =
  isJust $ do
    (t1, t2, (x, y)) <- hIntersectXY (hXY h1) (hXY h2)
    guard $ t1 >= 0
    guard $ t2 >= 0
    guard $ uncurry inRange r x
    guard $ uncurry inRange r y
    pure ()

part1 :: Range -> [Hailstone P3] -> Int
part1 r = countIf (uncurry $ intersectXYWithin r) . pairs

part2 :: [Hailstone P3] -> Int
part2 = error "part2"

tasks =
  Tasks
    2023
    24
    (CodeBlock 0)
    parser
    [ Assert "hIntersectXY" (Just (2, 1, (2, 2)))
        $ hIntersectXY @RInt (Hailstone (0, 0) (1, 1)) (Hailstone (2, 1) (0, 1))
    , Assert
        "hIntersectXY from task"
        (Just
           ( 47551182481306990 % 47919
           , 6631215366719602 % 15973
           , (3626653630736638815 % 15973, 3646362195818317820 % 15973)))
        $ hIntersectXY @RInt
            (Hailstone (468183773350185, 269960480220160) (-243, -42))
            (Hailstone (150661115836739, 133213164517594) (184, 229))
    , AssertExample "part 1" 2 $ part1 exampleRange
    , taskBlind (part1 taskRange) & taskPart 1
    , task part2 47 & taskPart 2
    ]
