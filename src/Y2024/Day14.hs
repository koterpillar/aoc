module Y2024.Day14
  ( tasks
  ) where

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as Text

import           AOC
import           Grid
import           Grid.Pixel
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

rDisplay' :: Position2 -> [Robot] -> LazyByteString
rDisplay' sz =
  displayPixels' color mempty
    . flip Map.union mids
    . mapFromListCount
    . map rPos
  where
    color :: Int -> Pixel
    color c
      | c < 0 = Pixel (200, 200, 200)
      | otherwise = Pixel (0, 128 - c' * 16, 0)
      where
        c' = fromInteger $ min 7 $ toInteger c
    Position2 mx my = mid sz
    mids =
      Map.fromList
        $ map (, -1)
        $ [Position2 mx y | y <- [0 .. pY sz - 1]]
            ++ [Position2 x my | x <- [0 .. pX sz - 1]]

rDisplay :: [Robot] -> LazyByteString
rDisplay = rDisplay' realSize

part1 :: Position2 -> [Robot] -> Int
part1 sz =
  countQuadrants sz
    . lbtraceF (rDisplay' sz)
    . map (rGo sz 100)
    . lbtraceF (rDisplay' sz)

variance :: [Int] -> Int
variance xs = sum $ map (\x -> (x - m) ^ 2) xs
  where
    m = sum xs `div` length xs

rVariance :: [Robot] -> Int
rVariance rs = variance (map (pX . rPos) rs) + variance (map (pY . rPos) rs)

part2 :: [Robot] -> Int
part2 rs =
  if length rs < 30
    then (-1)
    else go 0 rs
  where
    go :: Int -> [Robot] -> Int
    go n rs
      | rVariance rs < 400000 = lbtrace (rDisplay rs) n
      | otherwise = go (succ n) $ map (rGo realSize 1) rs

tasks =
  Tasks
    (AOC 2024 14)
    (CodeBlock 0)
    parser
    [ AssertExample "example size" 12 (part1 exampleSize)
    , taskBlind (part1 realSize) & taskPart 1
    , taskBlind part2 & taskPart 2
    ]
