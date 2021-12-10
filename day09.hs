import           Text.Parsec
import           Text.Parsec.Text

import           Data.Char        (ord)

import           Data.Function

import           Data.List

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Data.Maybe

import           Debug.Trace

import           AOC
import           Grid
import           Utils

digitsP :: Parser [Int]
digitsP = many1 $ (\c -> ord c - ord '0') <$> anyChar

type Floor = Map Position2 Int

type FPoint = (Position2, Int)

nearby :: Floor -> Position2 -> [FPoint]
nearby grid pt =
  mapMaybe
    (\d ->
       let pt' = walk d pt
        in (,) pt' <$> Map.lookup pt' grid)
    allDir4

lowPoints :: Floor -> [FPoint]
lowPoints grid = filter (uncurry lowest) $ Map.toList grid
  where
    lowest pt depth = all (> depth) $ map snd $ nearby grid pt

part1 :: Map Position2 Int -> Int
part1 = sum . map (risk . snd) . lowPoints

risk :: Int -> Int
risk depth = depth + 1

type Basin = Set FPoint

basins :: Floor -> [Basin]
basins grid = iterateSettle (map growBasin) initialBasins
  where
    initialBasins :: [Basin]
    initialBasins = map Set.singleton $ lowPoints grid
    growBasin :: Basin -> Basin
    growBasin b =
      Set.union b $ Set.fromList $ concatMap growBasinPoint $ Set.toList b
    growBasinPoint :: FPoint -> [FPoint]
    growBasinPoint (pt, d) =
      filter (\(_, d') -> d' > d && d' < 9) $ nearby grid pt

part2 :: Map Position2 Int -> Int
part2 =
  product .
  map Set.size . take 3 . reverse . sortBy (compare `on` Set.size) . basins

main :: IO ()
main = do
  processEI 9 (enumerate2 . parseLines digitsP) part1 15
  processEI 9 (enumerate2 . parseLines digitsP) part2 1134
