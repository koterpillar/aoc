module Y2021.Day19 where

import qualified Data.Map as Map
import qualified Data.Set as Set

import           AOC
import           Utils

pick :: Ord a => Set a -> [(a, Set a)]
pick xs = [(x, Set.delete x xs) | x <- Set.toList xs]

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
  Position3 (-x0 + x1 + x2) (-y0 + y1 + y2) (-z0 + z1 + z2)

scale3 :: Int -> Position3 -> Position3
scale3 s (Position3 x y z) = Position3 (x * s) (y * s) (z * s)

type View = Set Position3

type Orient = Position3 -> Position3

orientations :: [Orient]
orientations = [scale3 a . r | a <- [1, -1], r <- [id, r1, r2]]
  where
    r1 (Position3 x y z) = Position3 y z x
    r2 (Position3 x y z) = Position3 z x y

orientationsV :: View -> [(Orient, View)]
orientationsV v = [(o, Set.map o v) | o <- orientations]

unify :: View -> View -> Maybe View
unify = unify' 12

unify' :: Int -> View -> View -> Maybe View
unify' anchorsNeeded canon candidate =
  listToMaybe $ do
    (_, candidate') <- orientationsV candidate
    (candOrigin, candRest) <- pick candidate'
    (canonOrigin, canonRest) <- pick canon
    let translation = shiftFrom candOrigin canonOrigin
    let candRest' = Set.map translation candRest
    let matched = Set.size (Set.intersection canonRest candRest') + 1
    when (matched > 1) $ traceShowM matched
    guard $ matched >= anchorsNeeded
    pure $ Set.union canon candRest'

type Scene = Map Int View

part1 :: Scene -> Int
part1 = Set.size . reduceUnify unify . Map.elems

tasks =
  Tasks
    2021
    19
    parse
    [ let canon =
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
       in Assert
            "unify translation"
            (Just $ Set.insert (Position3 10 20 30) canon) $
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
