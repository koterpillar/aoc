{-# LANGUAGE FlexibleContexts #-}

module Y2023.Day23 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 (Either () Direction4)

instance GridItem (Either () Direction4) where
  showInGrid (Left ()) = '#'
  showInGrid (Right d) = showInGrid d

parser :: Parser Text Grid
parser = charGridP' ((Left <$> gridItemP) &| (Right <$> gridItemP))

directions :: Maybe (Either () Direction4) -> [Direction4]
directions Nothing          = allDir4
directions (Just (Left ())) = error "can't move from forest"
directions (Just (Right d)) = [d]

type App a = State (Set Position2) a

paths :: Grid -> [Set Position2]
paths g = ttrace (displayG g') paths
  where
    (paths, dead) = runState (go Set.empty start) Set.empty
    g' =
      Map.fromList [(d, 'x') | d <- Set.toList dead]
        `Map.union` Map.fromList
                      [ (p, showInGrid (i :: Int))
                      | (i, path) <- zipN 1 paths
                      , p <- Set.toList path
                      ]
        `Map.union` Map.map showInGrid g
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
    go :: Set Position2 -> Position2 -> App [Set Position2]
    go visited p
      | p == end = pure [visited]
      | otherwise = do
        alreadyDead <- gets $ Set.member p
        if alreadyDead
          then pure []
          else do
            let go' = go (Set.insert p visited)
            let ps = next visited p
            traceShowM (p, ps)
            rs <- traverse go' ps
            -- when (null rs) $ modify $ Set.insert p -- dead
            pure $ concat rs

part1 :: Grid -> Int
part1 = maximum . map Set.size . paths

tasks =
  Tasks
    2023
    23
    (CodeBlock 0)
    parser
    [ AssertExample "all paths" [94, 90, 86, 82, 82, 74]
        $ reverse . sort . map length . paths
    , task part1 94 & taskPart 1
    ]
