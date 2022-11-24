module Y2017.Day14 where

import qualified Data.Map.Strict as Map

import qualified Data.Set        as Set

import           Numeric

import qualified Data.Text       as Text

import           AOC
import           Graph
import           Grid
import           Utils
import           Y2017.Knot

showBin :: Int -> Text
showBin n = Text.pack $ showIntAtBase 2 (head . show) n ""

knotHashBin :: Text -> Text
knotHashBin = Text.concatMap hexToBin . knotHash

hexToBin :: Char -> Text
hexToBin = Text.justifyRight 4 '0' . showBin . fst . head . readHex . (: [])

type Grid = Grid2 ()

grid :: Text -> Grid
grid seed = mapFromList $ zip points (repeat ())
  where
    points = [Position2 x y | y <- [0 .. 127], x <- ones y]
    ones y = [x | (x, '1') <- zip [0 ..] bin]
      where
        bin = Text.unpack $ knotHashBin $ seed <> "-" <> tshow y

gridTopCorner :: Int -> Grid2 a -> Grid2 a
gridTopCorner n = Map.filterWithKey (\(Position2 x y) _ -> x < n && y < n)

mkGraph :: Grid -> Graph Position2
mkGraph ps = mapFromList neighbors
  where
    neighbors = do
      p1 <- Map.keys ps
      let p2s = filter (`Map.member` ps) (adjacent4 p1)
      pure (p1, Set.fromList p2s)

tasks = Tasks 2017 14 (Inline "flqrgnkx") (pureP grid) [Task length 8108]
