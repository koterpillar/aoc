module Y2021.Day25 where

import qualified Data.Map as Map

import           AOC
import           Grid
import           Utils

type Grid = (Grid2 Direction4, Position2)

step :: Grid -> Grid
step g = g & step' E & step' S

step' :: Direction4 -> Grid -> Grid
step' d (g0, b) = (foldl move g0 ps, b)
  where
    ps = [p | (p, c) <- mapToList g0, c == d]
    advance = wrapBounds (Position2 0 0, b) . walk d
    move g p =
      let p' = advance p
       in if mapMember p' g0
            then g
            else g & Map.delete p & mapInsert p' d

gtrace :: Grid -> Grid
gtrace = ttraceF (displayG . fst)

part1 :: Grid -> Int
part1 = length . iterateSettle step

tasks = Tasks 2021 25 (CodeBlock 0) parse [task part1 58]

mkGrid :: Grid2 Direction4 -> Grid
mkGrid g = (g, b)
  where
    (_, b) = boundsG g

parse :: Parser Text Grid
parse = mkGrid <$> charGridP
