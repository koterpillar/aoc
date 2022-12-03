module Y2021.Day25 where

import qualified Data.Map as Map

import           AOC
import           Grid
import           Utils

type Grid = (Grid2 Direction4, Position2)

step :: Grid -> Grid
step g = g & step' E & step' S

step' :: Direction4 -> Grid -> Grid
step' d (g0, p@(Position2 xmax ymax)) = (foldl move g0 ps, p)
  where
    ps = [p | (p, c) <- mapToList g0, c == d]
    advance p =
      let Position2 x' y' = walk d p
          x'' =
            if x' > xmax
              then 0
              else x'
          y'' =
            if y' > ymax
              then 0
              else y'
       in Position2 x'' y''
    move g p =
      let p' = advance p
       in if mapMember p' g0
            then g
            else g & Map.delete p & mapInsert p' d

instance GridItem Direction4 where
  showInGrid E = '>'
  showInGrid S = 'v'
  showInGrid _ = error "showInGrid"

gtrace = ttraceF (displayG . fst)

iw1 :: Int -> Grid -> Int
iw1 n g =
  let g' = step $ gtrace g
   in if g == g'
        then n
        else iw1 (n + 1) g'

part1 :: Grid -> Int
part1 = iw1 1

tasks = Tasks 2021 25 (CodeBlock 0) parse [Task part1 58]

mkGrid :: Grid2 Direction4 -> Grid
mkGrid g = (g, b)
  where
    (_, b) = boundsG g

parse :: Parser Text Grid
parse =
  mkGrid . fmap fromJust . Map.filter isJust . fromMatrixG <$>
  linesP &** charactersP &** cucumberP

cucumberP :: Parser Char (Maybe Direction4)
cucumberP = choiceP [('>', Just E), ('v', Just S), ('.', Nothing)]
