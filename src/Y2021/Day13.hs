module Y2021.Day13 where

import           Data.Map  (Map)
import qualified Data.Map  as Map

import           Data.Text (Text)
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

type Paper = Map Position2 ()

type Instructions = (Paper, [Fold])

applyFold :: Fold -> Paper -> Paper
applyFold = Map.mapKeys . applyFoldPoint

parseInstructions :: Text -> Instructions
parseInstructions = go . splitOn [""] . Text.lines
  where
    go [dots, folds] = (parseDots dots, map parseFold folds)
    go parts         = error $ "parseFolds: bad input: " <> show parts
    parseDots = Map.fromList . flip zip (repeat ()) . map parseDot
    parseDot ln =
      let [x, y] = Text.splitOn "," ln
       in Position2 (tread x) (tread y)
    parseFold ln =
      case Text.splitOn "=" (Text.drop 11 ln) of
        ["x", x] -> FoldX (tread x)
        ["y", y] -> FoldY (tread y)
        _        -> error $ "parseFold: bad input: " <> show ln

part1 :: Instructions -> Int
part1 (p, fs) = Map.size $ applyFold (head fs) p

displayPaper :: Paper -> Text
displayPaper = displayGrid d . mapToGrid
  where
    d (Just ()) = "#"
    d Nothing   = "."

part2 :: Instructions -> ()
part2 (p, fs) = ttrace (displayPaper $ foldl (flip applyFold) p fs) ()

tasks =
  Tasks
    2021
    13
    parseInstructions
    [ Assert "foldY" (Position2 1 4) (applyFoldPoint (FoldY 7) (Position2 1 10))
    , Assert "foldY" (Position2 1 4) (applyFoldPoint (FoldY 7) (Position2 1 4))
    , Task part1 17
    , Task part2 ()
    ]
