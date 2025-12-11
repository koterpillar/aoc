module Y2024.Day20
  ( tasks
  ) where

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as Text

import           AOC
import           Grid
import           Grid.Pixel
import           Path
import           Utils

data Item
  = Wall
  | Start
  | End
  deriving (Eq, Ord, Show, Enum, Bounded)

itemPixel :: Item -> Pixel
itemPixel Wall  = defaultPixel '#'
itemPixel Start = defaultPixel 'O'
itemPixel End   = defaultPixel 'X'

instance GridItem Item where
  showInGrid Wall  = '#'
  showInGrid Start = 'S'
  showInGrid End   = 'E'

type Grid = Grid2 Item

parser :: Parser Text Grid
parser = charGridP

isExample :: Grid -> Bool
isExample g =
  let (p1, p2) = boundsG g
      sz = 14
   in p2 `pointMinus` p1 == Position2 sz sz

gStart :: Grid -> Position2
gStart = mapFindValueE "gStart" (== Start)

gEnd :: Grid -> Position2
gEnd = mapFindValueE "gEnd" (== End)

gMoves :: Grid -> Position2 -> [Position2]
gMoves g p =
  filter (\p' -> Map.lookup p' g /= Just Wall) [walk d p | d <- allDir4]

gPath :: Grid -> [Position2]
gPath =
  fromJustE "gPath a*"
    <$> (aStarDepthGoal <$> gMoves <*> (manhattanDistance . gEnd) <*> gStart)

singleLinePath :: Grid -> Map Position2 Int
singleLinePath g
  | totalSize == walls + pathLength = Map.fromList $ zip p [0 ..]
  | otherwise =
    error
      $ "Not a single path: walls="
          <> show walls
          <> " pathLength="
          <> show pathLength
          <> " totalSize="
          <> show totalSize
  where
    p = gStart g : gPath g
    totalSize =
      let (Position2 x1 y1, Position2 x2 y2) = boundsG g
       in (x2 - x1 + 1) * (y2 - y1 + 1)
    walls = length g - 2
    pathLength = length p

part :: Int -> Int -> Grid -> Int
part cheatLength exampleCutoff g =
  length
    $ (if isE
         then traceShowId
         else id)
    $ sort cheats
  where
    isE = isExample $ lbtraceF (displayPixels' itemPixel mempty) g
    cutoff =
      if isE
        then exampleCutoff
        else 100
    p = singleLinePath g
    cheats = do
      (p1, i1) <- Map.toList p
      (p2, i2) <- Map.toList p
      let d = manhattanDistance p1 p2
      guard $ d <= cheatLength
      guard $ i1 < i2
      let saved = i2 - i1 - d
      (if isE
         then traceShowM
         else const (pure ()))
        (saved, p1, i1, p2, i2)
      guard $ saved >= cutoff
      pure saved

part1 :: Grid -> Int
part1 = part 2 1

part2 :: Grid -> Int
part2 = part 20 50

tasks =
  Tasks
    (AOC 2024 20)
    (CodeBlock 0)
    parser
    [ task part1 (14 + 14 + 2 + 4 + 2 + 3 + 1 + 1 + 1 + 1 + 1) & taskPart 1
    , task
        part2
        (32 + 31 + 29 + 39 + 25 + 23 + 20 + 19 + 12 + 14 + 12 + 22 + 4 + 3)
        & taskPart 2
    ]
