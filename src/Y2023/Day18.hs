module Y2023.Day18 where

import           Control.Parallel.Strategies (parMap, rpar)

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text

import           AOC
import           Grid
import           Memo
import           Utils

type Instr = (Direction4, Int)

mkInstr :: Direction4 -> Int -> Instr -> (Instr, Instr)
mkInstr d i hidden = ((d, i), hidden)

hiddenInstrP :: Parser Text Instr
hiddenInstrP =
  charactersP
    &* pureP (drop 1 . reverse . drop 2)
    &* unconsP
    &* (choiceEBP "0123" &= (pureP (Text.pack . reverse) &* hexP))

parser :: Parser Text [(Instr, Instr)]
parser =
  linesP
    &** (wordsP
           &* ap3P mkInstr (charP &* choiceEBP "RULD") integerP hiddenInstrP)

type Grid = Grid2 ()

type St = (Position2, Grid)

move :: Instr -> St -> St
move (d, n) (p, g) = (last ps, foldr (`Map.insert` ()) g ps)
  where
    ps = [walkN i d p | i <- [1 .. n]]

floodFill :: Grid -> Int
floodFill g = sum $ parMap rpar (traceShowId . insideLine) [ymin .. ymax]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = traceShowId $ boundsG g
    insideLine :: Int -> Int
    insideLine y = go y xmin False False
    go :: Int -> Int -> Bool -> Bool -> Int
    go y x n s
      | x > xmax = 0
      | otherwise =
        (if n || n' || s || s'
           then 1
           else 0)
          + go y (succ x) n' s'
      where
        n' =
          n
            /= (Map.member (Position2 x y) g
                  && Map.member (Position2 x (pred y)) g)
        s' =
          s
            /= (Map.member (Position2 x y) g
                  && Map.member (Position2 x (succ y)) g)

filledArea :: [Instr] -> Int
filledArea is = result
  where
    startP = Position2 0 0
    start = (startP, Map.singleton startP ())
    (_, trench) = foldl' (flip move) start is
    result = floodFill trench

part1 = filledArea . map fst

part2 = filledArea . map snd

tasks =
  Tasks
    2023
    18
    (CodeBlock 0)
    parser
    [task part1 62 & taskPart 1, task part2 952408144115 & taskPart 2]
