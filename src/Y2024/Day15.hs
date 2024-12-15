module Y2024.Day15
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Item
  = Wall
  | Box
  | Robot
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Item where
  showInGrid Wall  = '#'
  showInGrid Box   = 'O'
  showInGrid Robot = '@'

type Grid = (Position2, Grid2 Item)

type Input = (Grid, [Direction4])

parser :: Parser Text Input
parser =
  fmap mkInput
    $ tsplitP "\n\n"
        &* (charGridP
              &+ (pureP (Text.replace "\n" "") &* charactersP &** gridItemP))
  where
    mkInput (g', m) = ((r, g), m)
      where
        r = fromJustE "robot position" $ findTuple (== Robot) $ Map.toList g'
        g = Map.delete r g'

moveRobot :: Direction4 -> Grid -> Grid
moveRobot d (r, g) = go r' False
  where
    r' = walk d r
    go p pushing =
      case Map.lookup p g of
        Just Box -> go (walk d p) True
        Just Wall -> (r, g)
        Nothing ->
          ( r'
          , (if pushing
               then Map.insert p Box
               else id)
              $ Map.delete r' g)

displayGrid :: Grid -> Text
displayGrid (r, g) = displayG $ Map.insert r Robot g

gps :: Grid -> Int
gps = sum . map gpsCoord . Map.keys . Map.filter (== Box) . snd
  where
    gpsCoord (Position2 x y) = x + 100 * y

part1 :: Input -> Int
part1 (g, ms) = gps $ foldl' (flip moveRobot) g ms

tasks =
  Tasks
    2024
    15
    (CodeBlock 0)
    parser
    [task part1 2028 & taskScraper (CodeBlock 1), task part1 10092 & taskPart 1]
