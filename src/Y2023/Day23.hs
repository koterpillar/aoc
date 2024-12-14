{-# LANGUAGE FlexibleContexts #-}

module Y2023.Day23
  ( tasks
  ) where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Graph                      (dot)
import           Grid
import           Utils

type Grid = Grid2 (Either () Direction4)

instance GridItem (Either () Direction4) where
  showInGrid (Left ()) = '#'
  showInGrid (Right d) = showInGrid d

parser :: Parser Text Grid
parser = charGridP' ((Left <$> gridItemP) &| (Right <$> gridItemP))

displayPaths :: [Set Position2] -> Grid -> Text
displayPaths paths g =
  displayG
    $ Map.fromList
        [ (p, showInGrid (i :: Int))
        | (i, path) <- zipN 1 paths
        , p <- Set.toList path
        ]
        `Map.union` Map.map showInGrid g

paths :: Grid -> [Set Position2]
paths g = ttrace (displayPaths paths g) paths
  where
    paths = go Set.empty start
    start =
      fromSingleE "paths.start"
        $ filter (`Map.notMember` g) [Position2 x ymin | x <- [xmin .. xmax]]
    end =
      fromSingleE "paths.end"
        $ filter (`Map.notMember` g) [Position2 x ymax | x <- [xmin .. xmax]]
    b@(Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    next :: Set Position2 -> Position2 -> [Position2]
    next visited p =
      [ p'
      | insideBounds b p
      , d <- directions (Map.lookup p g)
      , let p' = walk d p
      , insideBounds b p'
      , Map.lookup p' g /= Just (Left ())
      , Set.notMember p' visited
      ]
    go :: Set Position2 -> Position2 -> [Set Position2]
    go visited p
      | p == end = [visited]
      | otherwise = concat rs
      where
        go' = go (Set.insert p visited)
        ps = next visited p
        rs = map go' ps
    directions :: Maybe (Either () Direction4) -> [Direction4]
    directions Nothing          = allDir4
    directions (Just (Left ())) = error "can't move from forest"
    directions (Just (Right d)) = [d]

part1 :: Grid -> Int
part1 = maximum . map Set.size . paths

data Edge t = Edge
  { eWeight :: Int
  , eTarget :: t
  } deriving (Show, Eq)

type Graph t = Map t [Edge t]

mkGraph :: Grid -> Graph Position2
mkGraph m =
  mapFromListS
    [ (a, [Edge 1 c])
    | x <- [xmin .. xmax]
    , y <- [ymin .. ymax]
    , let a = Position2 x y
    , walkable a
    , d <- allDir4
    , let c = walk d a
    , walkable c
    , insideBounds b c
    ]
  where
    b@(Position2 xmin ymin, Position2 xmax ymax) = boundsG m
    walkable p = Map.lookup p m /= Just (Left ())

optimizeStraight :: Ord t => Graph t -> Graph t
optimizeStraight m =
  case find ((== 2) . length . snd) $ Map.toList m of
    Nothing -> m
    Just (middle, [Edge w1 left, Edge w2 right]) ->
      Map.adjust (map $ edgePlus right) left
        $ Map.adjust (map $ edgePlus left) right
        $ Map.delete middle m
      where edgePlus pA e
              | eTarget e == middle = Edge (w1 + w2) pA
              | otherwise = e

optimize :: Ord t => Graph t -> Graph t
optimize = iterateSettleL optimizeStraight

allPaths ::
     forall t. Ord t
  => t
  -> t
  -> Graph t
  -> [Int]
allPaths start end g = go 0 Set.empty start
  where
    go cost visited p
      | p == end = [cost]
      | otherwise =
        concat $ do
          Edge w p' <- Map.findWithDefault [] p g
          guard $ Set.notMember p' visited
          pure $ go (cost + w) (Set.insert p' visited) p'

allPathsGrid :: Graph Position2 -> [Int]
allPathsGrid m = allPaths start end m
  where
    start = minimumOn pY $ Map.keys m
    end = maximumOn pY $ Map.keys m

mkdot :: Graph Position2 -> Text
mkdot = dot pv . Map.map (Set.fromList . map eTarget)
  where
    pv (Position2 x y) = "p_" <> tshow x <> "_" <> tshow y

part2 :: Grid -> Int
part2 = maximum . allPathsGrid . ttraceF mkdot . optimize . mkGraph

tasks =
  Tasks
    2023
    23
    (CodeBlock 0)
    parser
    [ AssertExample "all paths" [94, 90, 86, 82, 82, 74]
        $ reverse . sort . map length . paths
    , task part1 94 & taskPart 1
    , task part2 154 & taskPart 2
    ]
