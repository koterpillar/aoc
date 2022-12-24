module Y2022.Day23 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 ()

type Proposal = (Direction4, [Direction8])

proposals :: [Proposal]
proposals =
  [(N, [N_, NE, NW]), (S, [S_, SE, SW]), (W, [W_, NW, SW]), (E, [E_, NE, SE])]

countEmpty :: Grid2 a -> Int
countEmpty g = (xmax - xmin + 1) * (ymax - ymin + 1) - length g
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

type St = (Grid, [Proposal])

shift xs = tail xs ++ [head xs]

step :: St -> St
step (g, ps) = (g2, shift ps)
  where
    g2 = foldr (uncurry chooseMove) g $ Map.toList g1
    g1 =
      mapFromListS
        [(walk d p, [p]) | p <- Map.keys g, d <- maybeToList $ elfStep g ps p]
    chooseMove p' [p] = Map.delete p . Map.insert p' ()
    chooseMove _ _    = id

elfStep :: Grid -> [Proposal] -> Position2 -> Maybe Direction4
elfStep g ps p =
  listToMaybe $ do
    guard $ or [Map.member (walk d p) g | d <- allDir8]
    (d, dchecks) <- ps
    for_ dchecks $ \dcheck -> do
      let pcheck = walk dcheck p
      guard $ not $ Map.member pcheck g
    pure d

start :: Grid -> St
start g = (g, proposals)

part1 g = countEmpty $ fst $ iterateNL 10 step $ start g

part2 g = length $ iterateWhile2 ((/=) `on` fst) step $ start g

tasks = Tasks 2022 23 (CodeBlock 0) dotGridP [Task part1 110, Task part2 20]
