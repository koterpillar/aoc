module Y2024.Day12 where

import           Control.Monad
import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Grid
import           Utils

type Grid = Grid2 Char

parser :: Parser Text Grid
parser = charGridP

findArea1 :: Grid -> Maybe (Grid2 (), Grid)
findArea1 g =
  case Map.lookupMin g of
    Nothing -> Nothing
    Just (p, c) ->
      let r = flip execState Map.empty $ go c p
       in Just (r, Map.difference g r)
  where
    go :: Char -> Position2 -> State (Grid2 ()) ()
    go c p = do
      modify $ Map.insert p ()
      let ps1 = [walk d p | d <- allDir4]
      let ps2 = filter (\p' -> Map.lookup p' g == Just c) ps1
      ps <- filterM (gets . Map.notMember) ps2
      traverse_ (go c) ps

findAreas :: Grid -> [Grid2 ()]
findAreas = unfoldr findArea1

type PerimeterFn = forall a. Position2 -> Grid2 a -> Int

price :: PerimeterFn -> Grid2 () -> Int
price pf g = perimeter * area
  where
    area = Map.size g
    perimeter = sum $ map (`pf` g) $ Map.keys g

perimeter :: PerimeterFn
perimeter p g = countIf (`Map.notMember` g) [walk d p | d <- allDir4]

discountedPerimeter :: PerimeterFn
discountedPerimeter p g = countIf go allDir4
  where
    go d = Map.notMember pA g && not (Map.member p' g && Map.notMember pB g)
      where
        pA = walk d p
        p' = walk (turnRight d) p
        pB = walk d p'

part :: PerimeterFn -> Grid2 Char -> Int
part pf = sum . map (price pf) . findAreas

part1 :: Grid2 Char -> Int
part1 = part perimeter

part2 :: Grid2 Char -> Int
part2 = part discountedPerimeter

tasks =
  Tasks
    2024
    12
    (CodeBlock 3)
    parser
    [ task part1 1930 & taskPart 1
    , task part2 80 & taskScraper (CodeBlock 0)
    , task part2 1206 & taskPart 2
    ]
