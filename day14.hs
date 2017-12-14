import Control.Monad

import Data.Char
import Data.Foldable

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Numeric

import Knot
import Graph
import Utils

showBin :: Int -> String
showBin n = showIntAtBase 2 (head . show) n ""

knotHashBin :: String -> String
knotHashBin = concatMap hexToBin . knotHash

hexToBin :: Char -> String
hexToBin = pad' '0' 4 . showBin . fst . head . readHex . (:[])

type Grid = [[Bool]]

grid :: String -> Grid
grid seed = map (\i -> map binToEmpty $ knotHashBin $ seed ++ "-" ++ show i) [0..127]

binToEmpty '0' = False
binToEmpty '1' = True

showGrid :: Grid -> IO ()
showGrid = traverse_ $ putStrLn . map gridChar
  where
    gridChar True = '#'
    gridChar False = '.'

gridTopCorner :: Int -> [[a]] -> [[a]]
gridTopCorner n = take n . map (take n)

gridCount :: Grid -> Int
gridCount = sum . map (sum . map (\i -> if i then 1 else 0))

example :: IO Grid
example = read <$> readFile "day14example"

realGrid :: IO Grid
realGrid = read <$> readFile "day14real"

type GridSet = Set Position2

gridGraph :: Grid -> Graph Position2
gridGraph = mkGraph . mkMap

mkMap :: Grid -> Set Position2
mkMap = Set.fromList . concatMap (uncurry mkMapLine) . zip [0..]
  where
    mkMapLine :: Int -> [Bool] -> [Position2]
    mkMapLine y = mapMaybe (uncurry (mkPoint y)) . zip [0..]
    mkPoint y x False = Nothing
    mkPoint y x True = Just $ Position2 x y

mkGraph :: GridSet -> Graph Position2
mkGraph ps = Map.fromList neighbors
  where
    neighbors = do
      p1 <- Set.toList ps
      let p2s = filter (`Set.member` ps) [walk d p1 | d <- allDir4]
      pure (p1, Set.fromList p2s)
