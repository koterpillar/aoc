{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2021.Day19 where

import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Set             as Set

import           Linear.Matrix
import           Linear.V4
import           Linear.Vector        hiding (basis)

import           AOC
import           Utils

manhattanLength :: Position3 -> Int
manhattanLength (Position3 (V4 x y z _)) = abs x + abs y + abs z

newtype Position3 =
  Position3 (V4 Int)
  deriving (Eq, Ord, Show)

normP :: V4 Int -> Position3
normP (V4 x y z _) = Position3 (V4 x y z 1)

negateP :: Position3 -> Position3
negateP (Position3 a) = normP $ negated a

addP :: Position3 -> Position3 -> Position3
addP (Position3 a) (Position3 b) = normP $ a ^+^ b

subP :: Position3 -> Position3 -> Position3
subP a b = addP a (negateP b)

newtype Transform3 =
  Transform3 (M44 Int)
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

unify :: View -> View -> Maybe (Transform3, View)
unify = unify' 12

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
    traceM $ "unifying " <> show (length as) <> " with " <> show (length bs)
    r <- rotationsT
    a <- toList as
    b:br <- tails $ toList bs
    let t = unifyPoint r a b
    let trI = inverseT r `multiplyTT` t
    let b2a p = p `multiplyPT` trI
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
    let as' = setFromList $ map b2a $ b : br
    let matched = Set.intersection as as'
    let anchorsGot = length matched
    guard $ anchorsGot >= anchorsNeeded
    let result = as <> Set.map b2a bs
    traceMM
      [ "success"
      , "matched = " <> show anchorsGot
      , "result = " <> show (length result)
      ]
    pure (trI, result)

type Scene = Map Int View

unifyAll :: [View] -> Maybe ([Position3], View)
unifyAll [] = Nothing
unifyAll (canon:candidates) = do
  (result, trs) <- runWriterT $ go canon False candidates []
  pure (trs, result)
  where
    go ::
         View -- ^ canon
      -> Bool -- ^ any unification happened on this step?
      -> [View] -- ^ candidates
      -> [View] -- ^ unmatched
      -> WriterT [Position3] Maybe View
    go canon _ [] [] = pure canon
    go canon True [] unmatched = go canon False unmatched []
    go canon False [] unmatched = lift Nothing
    go canon u (candidate:rest) unmatched =
      case unify canon candidate of
        Nothing -> go canon u rest (candidate : unmatched)
        Just (t, canon') -> do
          tell [transformOrigin t]
          go canon' True rest unmatched

part1 :: Scene -> Int
part1 scene = length beacons
  where
    (_, beacons) = fromJustE "part1: cannot unify" $ unifyAll $ toList scene

part2 :: Scene -> Int
part2 scene =
  fromJustE "no unified beacons" $
  maybeMaximum [manhattanLength $ subP p1 p2 | p1 <- positions, p2 <- positions]
  where
    (positions, _) = fromJustE "part2: cannot unify" $ unifyAll $ toList scene

testBasis :: View
testBasis =
  setFromList
    [mkPosition 0 0 0, mkPosition 1 0 0, mkPosition 0 2 0, mkPosition 0 0 3]

tasks =
  Tasks
    2021
    19
    (CodeBlock 5)
    parse
    [ let p1 = mkPosition 1 2 3
          p2 = mkPosition 10 20 30
       in Assert "translationT" p2 $ multiplyPT p1 (translationT $ subP p2 p1)
    , Assert "rotationsT" 24 $
      length $ sort $ map (multiplyPT $ mkPosition 1 2 3) rotationsT
    , let canon = testBasis
          candidate =
            setFromList
              [ mkPosition 0 11 0
              , mkPosition 0 10 2
              , mkPosition 0 10 0
              , mkPosition 3 10 0
              , mkPosition 30 20 20
              ]
       in Assert
            "unify 1"
            (Just (mkPosition (-10) 0 0, Set.insert (mkPosition 10 20 30) canon)) $
          first transformOrigin <$> unify' 4 canon candidate
    , AssertExample "unify 0 and 1" True $
      (\(v1:v2:_) -> isJust $ unify v1 v2) . toList
    , Assert "orientation 1" basis $
      (map $ flip multiplyPT $ head rotationsT) basis
    , Task part1 79
    , Task part2 3621
    ]

parse :: Parser Text Scene
parse = mapFromList <$> lineGroupsP &** parseReport

parseReport :: Parser [Text] (Int, View)
parseReport =
  unconsP &* (parseScanner &= (setFromList <$> traverseP parsePosition3))

parseScanner :: Parser Text Int
parseScanner = wordsP &* pureP (!! 2) &* integerP

parsePosition3 :: Parser Text Position3
parsePosition3 =
  tsplitP "," &** integerP &*
  Parser
    (\case
       [x, y, z] -> Right (mkPosition x y z)
       other     -> Left $ "Expected 3 integers, found: " <> show other)
