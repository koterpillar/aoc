module Y2023.Day24 where

import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as Text

import           Data.Tuple.Extra

import           Data.Ratio

import           AOC
import           Utils

data V3 a =
  V3 a a a
  deriving (Eq, Ord, Show)

v3x :: V3 a -> a
v3x (V3 x _ _) = x

v3y :: V3 a -> a
v3y (V3 _ y _) = y

v3z :: V3 a -> a
v3z (V3 _ _ z) = z

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

v3ap :: Functor f => (f a -> b) -> f (V3 a) -> V3 b
v3ap f hs = V3 (a v3x) (a v3y) (a v3z)
  where
    a c = f $ fmap c hs

instance Foldable V3 where
  foldMap f (V3 x y z) = f x <> f y <> f z

instance Traversable V3 where
  traverse f (V3 x y z) = V3 <$> f x <*> f y <*> f z

instance Applicative V3 where
  pure x = V3 x x x
  V3 f g h <*> V3 x y z = V3 (f x) (g y) (h z)

type R3 = V3 Rational

type I3 = V3 Int

data Hailstone t = Hailstone
  { hPos :: t
  , hVel :: t
  } deriving (Eq, Ord, Show)

instance Functor Hailstone where
  fmap f (Hailstone p v) = Hailstone (f p) (f v)

class Manifold t c where
  tMulPlus :: t -> c -> c -> c

instance Manifold Rational Rational where
  tMulPlus t v x = t * v + x

instance (Manifold a b, Manifold a c) => Manifold a (b, c) where
  tMulPlus t (v1, v2) (x1, x2) = (tMulPlus t v1 x1, tMulPlus t v2 x2)

hAt :: Manifold t c => t -> Hailstone c -> c
hAt t (Hailstone p v) = tMulPlus t v p

parser :: Parser Text [Hailstone R3]
parser = linesP &** tsplitP " @ " &* ap2P Hailstone p3p p3p
  where
    p3p = tsplitP "," &* ap3P V3 ratioP ratioP ratioP
    ratioP = fromIntegral <$> readP @Integer

type Range = (Rational, Rational)

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

hXY :: Hailstone (V3 a) -> Hailstone (a, a)
hXY = fmap (\(V3 x y _) -> (x, y))

intersectXYWithin :: Range -> Hailstone R3 -> Hailstone R3 -> Bool
intersectXYWithin r h1 h2 =
  isJust $ do
    (t1, t2, (x, y)) <- hIntersectXY (hXY h1) (hXY h2)
    guard $ t1 >= 0
    guard $ t2 >= 0
    guard $ uncurry inRange r x
    guard $ uncurry inRange r y
    pure ()

part1 :: Range -> [Hailstone R3] -> Int
part1 r = countIf (uncurry $ intersectXYWithin r) . pairs

h3Int :: [Hailstone R3] -> [Hailstone I3]
h3Int = fmap $ fmap $ fmap floor

possibleSpeed :: Hailstone Int -> Hailstone Int -> Maybe (Set Int)
possibleSpeed (Hailstone pa va) (Hailstone pb vb)
  | va == vb =
    let d = abs $ pb - pa
     in Just
          $ Set.fromList
              [ v
              | t <- [1 .. floor $ sqrt $ fromIntegral d]
              , let (v0, r) = d `divMod` t
              , r == 0
              , v1 <- [v0, d `div` v0]
              , v <- [va - v1, va + v1]
              ]
  | otherwise = Nothing

part2Test :: [Hailstone R3] -> Text
part2Test hs0 =
  tshow [("x", speedGcd v3x), ("y", speedGcd v3y), ("z", speedGcd v3z)]
  where
    speedGcd f =
      traceShowId
        $ foldr1 Set.intersection
        $ mapMaybe (uncurry possibleSpeed)
        $ pairs
        $ fmap (fmap f) hs
    hs = h3Int hs0

part2 :: [Hailstone R3] -> Int
part2 = error "part 2"

tasks =
  Tasks
    2023
    24
    (CodeBlock 0)
    parser
    [ Assert "hIntersectXY" (Just (2, 1, (2, 2)))
        $ hIntersectXY @Rational
            (Hailstone (0, 0) (1, 1))
            (Hailstone (2, 1) (0, 1))
    , Assert
        "hIntersectXY from task"
        (Just
           ( 47551182481306990 % 47919
           , 6631215366719602 % 15973
           , (3626653630736638815 % 15973, 3646362195818317820 % 15973)))
        $ hIntersectXY @Rational
            (Hailstone (468183773350185, 269960480220160) (-243, -42))
            (Hailstone (150661115836739, 133213164517594) (184, 229))
    , AssertExample "part 1" 2 $ part1 exampleRange
    , taskBlind (part1 taskRange) & taskPart 1
    , Assert "possible speed 3 0" (Just $ Set.fromList [-3, -1, 1, 3])
        $ possibleSpeed (Hailstone 0 0) (Hailstone 3 0)
    , Assert "possible speed 3 1" (Just $ Set.fromList [-2, 0, 2, 4])
        $ possibleSpeed (Hailstone 0 1) (Hailstone 3 1)
    , taskBlind part2Test
    , task part2 47 & taskPart 2
    ]
