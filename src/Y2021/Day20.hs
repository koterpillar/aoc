module Y2021.Day20 where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Bit
import           Grid
import           Utils

data Grid =
  Grid
    { inside  :: !(Grid2 ())
    , outside :: !Bool
    }

instructionSize :: Int
instructionSize = 2 ^ 9

type Instructions = Map Int Bool

dotP :: Parser Char Bool
dotP = choiceP [('.', False), ('#', True)]

dotsP :: Parser Text [Bool]
dotsP = charactersP &** dotP

mkGrid :: [[Bool]] -> Grid
mkGrid values = Grid {..}
  where
    inside = Map.map (const ()) $ Map.filter id $ fromMatrixG values
    outside = False

mkInstructions :: [Bool] -> Instructions
mkInstructions = Map.fromList . zip [0 ..]

instructionFor :: Instructions -> Int -> Bool
instructionFor instructions n =
  fromJustE ("no instructions for " <> show n) $ Map.lookup n instructions

parse :: Parser Text (Instructions, Grid)
parse =
  lineGroupsP &* pairP &*
  (pureP Text.concat &* (mkInstructions <$> dotsP) &=
   (mkGrid <$> traverseP dotsP))

gridBit :: Position2 -> Grid -> Bit
gridBit p g
  | insideBounds (gridBounds g) p =
    case Map.lookup p (inside g) of
      Nothing -> O
      Just () -> I
  | otherwise =
    if outside g
      then I
      else O

gridBounds :: Grid -> (Position2, Position2)
gridBounds = boundsG . inside

gridDisplay :: Grid -> Text
gridDisplay (Grid inside outside) =
  "Outside: " <>
  if outside
    then "#"
    else "." <> "\n" <> displayG inside

gridCount :: Grid -> Int
gridCount (Grid _ True)  = error "infinite"
gridCount (Grid m False) = Map.size m

step :: Instructions -> Grid -> Grid
step instructions grid = ttraceF gridDisplay result
  where
    result = Grid inside' outside'
    inside' =
      Map.fromList $ do
        let (Position2 xmin ymin, Position2 xmax ymax) = gridBounds grid
        y <- [ymin - 3 .. ymax + 3]
        x <- [xmin - 3 .. xmax + 3]
        let bits =
              bitsValue
                [ gridBit (Position2 (x + dx) (y + dy)) grid
                | dy <- [-1 .. 1]
                , dx <- [-1 .. 1]
                ]
        guard $ instructionFor instructions bits
        pure (Position2 x y, ())
    outside' =
      instructionFor instructions $
      if outside grid
        then instructionSize - 1
        else 0

lightAfter :: Int -> (Instructions, Grid) -> Int
lightAfter steps (instructions, grid) =
  gridCount $ iterateN steps (step instructions) $ ttraceF gridDisplay grid

part1 = lightAfter 2

part2 = lightAfter 50

tasks :: Tasks
tasks = Tasks 2021 20 parse [Task part1 35, Task part2 3351]
