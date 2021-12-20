module Y2021.Day19 where

import qualified Data.Map as Map
import qualified Data.Set as Set

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

data Position3 =
  Position3
    { pX :: !Int
    , pY :: !Int
    , pZ :: !Int
    }
  deriving (Eq, Ord)

instance Show Position3 where
  show (Position3 x y z) =
    "(" <> show x <> "," <> show y <> "," <> show z <> ")"

shiftFrom :: Position3 -> Position3 -> Position3 -> Position3
shiftFrom (Position3 x0 y0 z0) (Position3 x1 y1 z1) (Position3 x2 y2 z2) =
  Position3 (x2 - x0 + x1) (y2 - y0 + y1) (z2 - z0 + z1)

scale3 :: Int -> Position3 -> Position3
scale3 s (Position3 x y z) = Position3 (x * s) (y * s) (z * s)

type View = Set Position3

type Orient = Position3 -> Position3

orientations :: [Orient]
orientations =
  [r2 c2 . r1 c1 . r3 c3 | c2 <- [0 .. 3], c1 <- [0 .. 1], c3 <- [0 .. 2]]
  where
    r3 0 p                 = p
    r3 n (Position3 x y z) = r3 (n - 1) $ Position3 y z x
    r1 0 p                 = p
    r1 _ (Position3 x y z) = Position3 (negate x) (negate y) z
    r2 0 p                 = p
    r2 n (Position3 x y z) = r2 (n - 1) $ Position3 x z (negate y)

orientationsV :: View -> [(Orient, View)]
orientationsV v = [(o, Set.map o v) | o <- orientations]

unify :: View -> View -> Maybe View
unify = unify' 12

setTails :: Ord a => Set a -> [(a, Set a)]
setTails = map (second Set.fromList) . pickFromTails . Set.toList
  where
    pickFromTails as = [(a, as) | a:as <- tails as]

unify' :: Int -> View -> View -> Maybe View
unify' anchorsNeeded canon candidate =
  listToMaybe $ do
    traceM $
      "unifying " <>
      show (Set.size canon) <> " with " <> show (Set.size candidate)
    o <- orientations
    (candOrigin, candRest) <- setPick $ Set.map o candidate
    (canonOrigin, canonRest) <- setPick canon
    let translation = shiftFrom candOrigin canonOrigin
    let candRestT = Set.map translation candRest
    let matched = Set.intersection canonRest candRestT
    let anchorsGot = Set.size matched + 1
    guard $ anchorsGot >= anchorsNeeded
    let result = Set.union canon candRestT
    traceM $
      "success, matched = " <>
      show anchorsGot <> ", result = " <> show (Set.size result)
    pure result

type Scene = Map Int View

part1 :: Scene -> Int
part1 = Set.size . reduceUnify unify . Map.elems

tasks =
  Tasks
    2021
    19
    parse
    [ Assert "orientations" 24 $
      length $ sort $ map ($ Position3 1 2 3) orientations
    , let canon =
            Set.fromList
              [ Position3 0 0 0
              , Position3 1 0 0
              , Position3 0 2 0
              , Position3 0 0 3
              ]
          candidate =
            Set.fromList
              [ Position3 0 10 0
              , Position3 0 11 0
              , Position3 0 10 2
              , Position3 3 10 0
              , Position3 30 20 20
              ]
       in Assert "unify 1" (Just $ Set.insert (Position3 10 20 30) canon) $
          unify' 4 canon candidate
    , AssertExample
        "unify 0 and 1"
        True
        (isJust .
         uncurry unify . \scene ->
           (fromJust (Map.lookup 0 scene), fromJust (Map.lookup 1 scene)))
    , Task part1 79
    ]

parse :: Parser Text Scene
parse = Map.fromList <$> linesP &* splitP [""] &** parseReport

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
       [x, y, z] -> Right (Position3 x y z)
       other -> Left $ "Expected 3 integers, found: " <> show other)
