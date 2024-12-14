module Y2024.Day14
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Robot = Robot
  { rPos :: Position2
  , rVel :: Position2
  } deriving (Ord, Eq, Show)

rGo :: Position2 -> Int -> Robot -> Robot
rGo (Position2 sx sy) n (Robot (Position2 x y) v@(Position2 vx vy)) =
  Robot (Position2 (go sx n x vx) (go sy n y vy)) v
  where
    go s n p v = (p + n * v) `mod` s

parser :: Parser Text [Robot]
parser =
  linesP
    &** (wordsP &* pureP (map $ Text.drop 2) &* ap2P Robot position2P position2P)

exampleSize :: Position2
exampleSize = Position2 11 7

realSize :: Position2
realSize = Position2 101 103

mid :: Position2 -> Position2
mid (Position2 x y) = Position2 (d2 x) (d2 y)
  where
    d2 x = x `div` 2

quadrant :: Position2 -> Robot -> Maybe (Int, Int)
quadrant sz (Robot (Position2 x y) _) = (,) <$> q mx x <*> q my y
  where
    Position2 mx my = mid sz
    q m p =
      case signum $ p - m of
        0 -> Nothing
        n -> Just n

countQuadrants :: Position2 -> [Robot] -> Int
countQuadrants sz =
  product . traceShowId . mapFromListCount . mapMaybe (quadrant sz)

rDisplay :: Position2 -> [Robot] -> Text
rDisplay sz =
  displayG
    . flip Map.union mids
    . fmap showInGrid
    . mapFromListCount @Position2 @Int
    . map rPos
  where
    Position2 mx my = mid sz
    mids =
      Map.fromList
        $ map (, ' ')
        $ [Position2 mx y | y <- [0 .. pY sz - 1]]
            ++ [Position2 x my | x <- [0 .. pX sz - 1]]

part1 :: Position2 -> [Robot] -> Int
part1 sz =
  countQuadrants sz
    . ttraceF (rDisplay sz)
    . map (rGo sz 100)
    . ttraceF (rDisplay sz)

tasks =
  Tasks
    2024
    14
    (CodeBlock 0)
    parser
    [ AssertExample "example size" 12 (part1 exampleSize)
    , taskBlind (part1 realSize) & taskPart 1
    ]
