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

proposalsCycle :: [[Proposal]]
proposalsCycle = map (take (length proposals)) $ inits $ cycle proposals

countEmpty :: Grid2 a -> Int
countEmpty g = (xmax - xmin + 1) * (ymax - ymin + 1) - length g
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g

step :: Grid -> [Proposal] -> Grid
step g ps = ttraceF displayG $ foldr (uncurry chooseMove) g $ Map.toList g1
  where
    g1 =
      mapFromListWith (++) $
      catMaybes [(, [p]) <$> elfStep g ps p | p <- Map.keys g]
    chooseMove p' [p] = Map.delete p . Map.insert p' ()
    chooseMove _ _    = id

elfStep :: Grid -> [Proposal] -> Position2 -> Maybe Position2
elfStep g ps p =
  listToMaybe $ do
    (d, dchecks) <- ps
    for_ dchecks $ \dcheck -> do
      let pcheck = walk dcheck p
      guard $ not $ Map.member pcheck g
    pure $ walk d p

part1 g = countEmpty $ foldl' step g $ take 10 proposalsCycle

tasks = Tasks 2022 23 (CodeBlock 0) dotGridP [Task part1 110]
