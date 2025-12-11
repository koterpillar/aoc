module Y2021.Day13
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Fold
  = FoldX Int
  | FoldY Int
  deriving (Show)

pfold :: Int -> Int -> Int
pfold p' p =
  let ans = p' - abs (p - p')
   in if ans < 0 || ans >= p'
        then error ("cannot fold " <> show p <> " over " <> show p')
        else ans

applyFoldPoint :: Fold -> Position2 -> Position2
applyFoldPoint (FoldX xc) (Position2 x y) = Position2 (pfold xc x) y
applyFoldPoint (FoldY yc) (Position2 x y) = Position2 x (pfold yc y)

type Paper = Grid2 ()

type Instructions = (Paper, [Fold])

applyFold :: Fold -> Paper -> Paper
applyFold = Map.mapKeys . applyFoldPoint

parseInstructions :: Parser Text Instructions
parseInstructions = lineGroupsP &* parseDots &+ traverseP parseFold
  where
    parseDots = mapFromList . flip zip (repeat ()) <$> traverseP parseDot
    parseDot = tsplitP "," &* ap2P Position2 integerP integerP
    parseFold =
      pureP (Text.drop 11)
        &* tsplitP "="
        &* ap2P ($) (choiceP [("x", FoldX), ("y", FoldY)]) integerP

part1 :: Instructions -> Int
part1 (p, fs) = length $ applyFold (head fs) p

part2 :: Instructions -> Text
part2 (p, fs) = displayG $ foldl (flip applyFold) p fs

tasks =
  Tasks
    (AOC 2021 13)
    (CodeBlock 1)
    parseInstructions
    [ Assert "foldY" (Position2 1 4) (applyFoldPoint (FoldY 7) (Position2 1 10))
    , Assert "foldY" (Position2 1 4) (applyFoldPoint (FoldY 7) (Position2 1 4))
    , task part1 17 & taskPart 1
    , taskBlind part2
    ]
