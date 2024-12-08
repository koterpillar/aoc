module Y2024.Day08 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

parser :: Parser Text (Grid2 Char)
parser = charGridP

pairs :: Grid2 Char -> [(Position2, Position2)]
pairs g = do
  c <- nubOrd $ toList g
  guard $ c /= '.'
  let ps = Map.keys $ Map.filter (== c) g
  (p1:ps') <- tails ps
  p2 <- ps'
  pure (p1, p2)

type AntinodesFn
  = (Position2, Position2) -> Position2 -> Position2 -> [Position2]

antinode1 :: AntinodesFn
antinode1 b p1 p2 = filter (insideBounds b) $ outside ++ inside
  where
    dp = p2 `pointMinus` p1
    outside = [p2 `pointPlus` dp, p1 `pointMinus` dp]
    inside =
      fromMaybe [] $ do
        (dx3, 0) <- Just $ pX dp `divMod` 3
        (dy3, 0) <- Just $ pY dp `divMod` 3
        let dp3 = Position2 dx3 dy3
        pure [p1 `pointPlus` dp3, p2 `pointMinus` dp3]

antinode2 :: AntinodesFn
antinode2 b p1 p2 = res
  where
    dp@(Position2 dx dy) = p2 `pointMinus` p1
    c = gcd dx dy
    dpc = Position2 (dx `div` c) (dy `div` c)
    tw = takeWhile $ insideBounds b
    res =
      [[0 ..], [-1,-2 ..]]
        >>= (tw . map (\d -> p1 `pointPlus` (d `pointM` dpc)))

antinodes :: AntinodesFn -> Grid2 Char -> Set Position2
antinodes af g =
  Set.fromList $ do
    let b = boundsG g
    (p1, p2) <- pairs g
    af b p1 p2

part :: AntinodesFn -> Grid2 Char -> Int
part af g =
  ttrace
    (displayG $ flip Map.union g $ Map.fromList (map (, '#') (Set.toList as)))
    Set.size
    as
  where
    as = antinodes af g

part1 :: Grid2 Char -> Int
part1 = part antinode1

part2 :: Grid2 Char -> Int
part2 = part antinode2

tasks =
  Tasks
    2024
    8
    (CodeBlock 0)
    parser
    [task part1 14 & taskPart 1, task part2 34 & taskPart 2]
