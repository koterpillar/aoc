{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2021.Day19 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Linear.Matrix
import           Linear.V4
import           Linear.Vector

import           AOC
import           Utils

pick :: [a] -> [(a, [a])]
pick []     = []
pick (x:xs) = (x, xs) : map (second (x :)) (pick xs)

setPick :: Ord a => Set a -> [(a, Set a)]
setPick xs = [(x, Set.delete x xs) | x <- Set.toList xs]

reduceUnify :: Show a => (a -> a -> Maybe a) -> [a] -> a
reduceUnify _ [] = error "reduceUnify: empty list"
reduceUnify _ [x] = x
reduceUnify fn (x:xs) = reduceUnify fn $ go xs
  where
    go [] = error $ "reduceUnify: " <> show x <> " wasn't unified with anything"
    go (y:ys) =
      case fn x y of
        Nothing -> y : go ys
        Just z  -> z : ys

manhattanLength :: Position3 -> Int
manhattanLength (Position3 (V4 x y z _)) = abs x + abs y + abs z

type TMatrix = M44 Int

type PMatrix = V4 Int

newtype Position3 =
  Position3
    { pMatrix :: PMatrix
    }
  deriving (Eq, Ord, Show)

normP :: PMatrix -> Position3
normP (V4 x y z _) = Position3 (V4 x y z 1)

negateP :: Position3 -> Position3
negateP (Position3 a) = normP $ negated a

addP :: Position3 -> Position3 -> Position3
addP (Position3 a) (Position3 b) = normP $ a ^+^ b

subP :: Position3 -> Position3 -> Position3
subP a b = addP a (negateP b)

newtype Transform3 =
  Transform3
    { tMatrix :: TMatrix
    }
  deriving (Eq, Show)

matrixInverse :: Num a => [[a]] -> [[a]]
matrixInverse m = error "matrixInverse: not implemented"

multiplyPT :: Position3 -> Transform3 -> Position3
multiplyPT (Position3 p) (Transform3 t) = Position3 $ p *! t

multiplyTT :: Transform3 -> Transform3 -> Transform3
multiplyTT (Transform3 a) (Transform3 b) = Transform3 $ a !*! b

identityT :: Transform3
identityT = Transform3 identity

powerT :: Int -> Transform3 -> Transform3
powerT 0 _ = identityT
powerT n t
  | n < 0 = error $ "powerT: negative power " <> show n
  | otherwise = t `multiplyTT` powerT (n - 1) t

inverseT :: Transform3 -> Transform3
inverseT (Transform3 m) =
  Transform3 $ ffmap round $ inv44 $ ffmap fromIntegral m
  where
    ffmap = fmap . fmap

mkPosition :: Int -> Int -> Int -> Position3
mkPosition x y z = Position3 $ V4 x y z 1

origin :: Position3
origin = mkPosition 0 0 0

transformOrigin :: Transform3 -> Position3
transformOrigin = multiplyPT origin

basis :: [Position3]
basis = [mkPosition 1 0 0, mkPosition 0 1 0, mkPosition 0 0 1]

rotationsT :: [Transform3]
rotationsT =
  [ powerT c2 t2 `multiplyTT` powerT c1 t1 `multiplyTT` powerT c3 t3
  | c2 <- [0 .. 3]
  , c1 <- [0 .. 1]
  , c3 <- [0 .. 2]
  ]
  where
    t3 = Transform3 $ V4 (V4 0 1 0 0) (V4 0 0 1 0) (V4 1 0 0 0) (V4 0 0 0 1)
    t1 =
      Transform3 $ V4 (V4 (-1) 0 0 0) (V4 0 (-1) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
    t2 = Transform3 $ V4 (V4 1 0 0 0) (V4 0 0 1 0) (V4 0 (-1) 0 0) (V4 0 0 0 1)

translationT :: Position3 -> Transform3
translationT (Position3 (V4 dx dy dz _)) =
  Transform3 $ V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 dx dy dz 1)

type View = Set Position3

unify :: View -> View -> Maybe View
unify v1 v2 = snd <$> unify' 12 v1 v2

setTails :: Ord a => Set a -> [(a, Set a)]
setTails = map (second Set.fromList) . pickFromTails . Set.toList
  where
    pickFromTails as = [(a, as) | a:as <- tails as]

-- unifyPoint r a b = t ==> a * t^-1 * r = b, a = b * r^-1 * t
unifyPoint :: Transform3 -> Position3 -> Position3 -> Transform3
unifyPoint r a b =
  if b == b'
    then t
    else error $
         "unifyPoint: b = " <>
         show b <>
         " doesn't match b' = " <>
         show b' <> " a = " <> show a <> " t = " <> show t <> " r = " <> show r
  where
    t = translationT $ subP a $ multiplyPT b $ inverseT r
    b' = a `multiplyPT` inverseT t `multiplyPT` r

traceMM :: Applicative f => [String] -> f ()
traceMM = traceM . intercalate ", "

unify' :: Int -> View -> View -> Maybe (Transform3, View)
unify' anchorsNeeded as bs =
  listToMaybe $ do
    traceM $ "unifying " <> show (Set.size as) <> " with " <> show (Set.size bs)
    (a, ars) <- setPick as
    r <- rotationsT
    (b, brs) <- setPick bs
    let t = unifyPoint r a b
    let trI = inverseT r `multiplyTT` t
    let b2a b = b `multiplyPT` trI
    let a' = b2a b
    when (a' /= a) $ do
      traceMM
        [ "b = " <> show b
        , "a' = " <> show a'
        , "a = " <> show a
        , "t = " <> show t
        , "r = " <> show r
        ]
      error "unify translation back failed"
    let as' = Set.map b2a bs
    let matched = Set.intersection as as'
    let anchorsGot = Set.size matched
    guard $ anchorsGot >= anchorsNeeded
    let result = Set.union as as'
    traceMM
      [ "success"
      , "matched = " <> show anchorsGot
      , "result = " <> show (Set.size result)
      ]
    pure (trI, result)

type Scene = Map Int View

part1 :: Scene -> Int
part1 = Set.size . reduceUnify unify . Map.elems

scannerPositions :: Scene -> [Position3]
scannerPositions scene =
  let (canon:candidates) = Map.elems scene
   in go canon candidates []
    -- go canon candidates unmatched -> positions
  where
    go :: View -> [View] -> [View] -> [Position3]
    go canon [] [] = []
    go canon [] unmatched = go canon unmatched []
    go canon (candidate:rest) unmatched =
      case unify' 12 canon candidate of
        Nothing          -> go canon rest (candidate : unmatched)
        Just (t, canon') -> transformOrigin t : go canon' rest unmatched

part2 :: Scene -> Int
part2 scene =
  fromJustE "no unified beacons" $
  maybeMaximum [manhattanLength $ subP p1 p2 | p1 <- positions, p2 <- positions]
  where
    positions = scannerPositions scene

testBasis :: View
testBasis =
  Set.fromList
    [mkPosition 0 0 0, mkPosition 1 0 0, mkPosition 0 2 0, mkPosition 0 0 3]

tasks =
  Tasks
    2021
    19
    parse
      {-let p1 = mkPosition 1 2 3
          p2 = mkPosition 10 20 30
          in Assert "translationT" p2 $ multiplyPT p1 (translationT p1 p2)
    ,-}
    [ Assert "rotationsT" 24 $
      length $ sort $ map (multiplyPT $ mkPosition 1 2 3) rotationsT
    , let canon = testBasis
          candidate =
            Set.fromList
              [ mkPosition 0 10 0
              , mkPosition 0 11 0
              , mkPosition 0 10 2
              , mkPosition 3 10 0
              , mkPosition 30 20 20
              ]
       in Assert
            "unify 1"
            (Just (mkPosition (-10) 0 0, Set.insert (mkPosition 10 20 30) canon)) $
          first transformOrigin <$> unify' 4 canon candidate
    , AssertExample "unify 0 and 1" True $
      (\(v1:v2:_) -> isJust $ unify v1 v2) . Map.elems
    -- , Assert "orientation 1" basis $ (map $ rotationsT !! 1) basis
    -- , let canon = Set.map (p3add $ mkPosition 10 0 50) testBasis
    --       candidate =
    --         Set.map (p3add (mkPosition 0 80 0) . (rotationsT !! 1)) testBasis
    --    in Assert "unify translation" (Just (mkPosition 10 (-80) 50)) $
    --       fst <$> unify' 4 canon candidate
    -- , Task part1 79
    , Task part2 3621
    ]

parse :: Parser Text Scene
parse = Map.fromList <$> lineGroupsP &** parseReport

parseReport :: Parser [Text] (Int, View)
parseReport =
  unconsP &* (parseScanner &= (Set.fromList <$> traverseP parsePosition3))

parseScanner :: Parser Text Int
parseScanner = wordsP &* pureP (!! 2) &* integerP

parsePosition3 :: Parser Text Position3
parsePosition3 =
  tsplitP "," &** integerP &*
  Parser
    (\case
       [x, y, z] -> Right (mkPosition x y z)
       other -> Left $ "Expected 3 integers, found: " <> show other)
