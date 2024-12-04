module Y2024.Day04 where

import           Data.Char

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Linear    (Trace (diagonal))
import           Utils

parser :: Parser Text (Grid2 Char)
parser = charGridP

part1 g =
  sum $ do
    p <- Map.keys g
    d <- allDir8
    pure $ findFromDir "XMAS" p d g

findFromDir :: String -> Position2 -> Direction8 -> Grid2 Char -> Int
findFromDir "" _ _ _ = 1
findFromDir (c:cs) p d g =
  case Map.lookup p g of
    Just c'
      | c == c' -> findFromDir cs (walk d p) d g
    _ -> 0

diagonals :: ([Direction8], [Direction8])
diagonals = ([NE, SW], [NW, SE])

isMS :: [Maybe Char] -> Bool
isMS [Just 'M', Just 'S'] = True
isMS [Just 'S', Just 'M'] = True
isMS _                    = False

findXMas :: Grid2 Char -> [Position2]
findXMas g =
  nubOrd $ do
    p <- Map.keys g
    guard $ Map.lookup p g == Just 'A'
    let (a1, a2) = diagonals
    let p1s = map (\d -> Map.lookup (walk d p) g) a1
    guard $ isMS p1s
    let p2s = map (\d -> Map.lookup (walk d p) g) a2
    guard $ isMS p2s
    pure p

part2 :: Grid2 Char -> Int
part2 = length . findXMas

tasks =
  Tasks
    2024
    4
    (CodeBlock 1)
    parser
    [task part1 18 & taskPart 1, task part2 9 & taskPart 2]
