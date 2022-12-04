import           Data.List
import           Data.List.Utils

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.Maybe

import           Debug.Trace

import           Utils

type Grid = Map Position2 Char

startPos :: Grid -> Position2
startPos g =
  fst $
  head $
  mapMaybe (\x -> (,) (Position2 x 0) <$> Map.lookup (Position2 x 0) g) [0 ..]

path :: Grid -> String
path g = go g (startPos g) S

pathLetters :: Grid -> String
pathLetters = filter (`elem` ['A' .. 'Z']) . path

pathLen :: Grid -> Int
pathLen = length . path

go :: Grid -> Position2 -> Direction4 -> String
go g pos dir =
  case Map.lookup pos g of
    Nothing  -> []
    Just '+' -> '+' : turnFrom pos dir
    Just ltr -> ltr : keepGoing pos dir
  where
    keepGoing pos dir = go g (walk dir pos) dir
    turnFrom pos dir =
      let otherDirs =
            Set.toList $ Set.delete (reverse4 dir) $ Set.fromList allDir4
          nextPts = map (`walk` pos) otherDirs
          nextChars = map (`Map.lookup` g) nextPts
          candidates = filter canTurn $ zip3 otherDirs nextPts nextChars
          canTurn (E, _, Just '|') = False
          canTurn (W, _, Just '|') = False
          canTurn (N, _, Just '-') = False
          canTurn (S, _, Just '-') = False
          canTurn (_, _, Nothing)  = False
          canTurn _                = True
       in case candidates of
            [(newDir, newPos, _)] -> go g newPos newDir
            _                     -> error $ show candidates

readGrid :: IO Grid
readGrid = parseGrid <$> readLines

parseGrid :: [String] -> Grid
parseGrid = foldr (uncurry parseLine) Map.empty . zip [0 ..]
  where
    parseLine :: Int -> String -> Grid -> Grid
    parseLine y str g = foldr (uncurry parseChar) g $ zip [0 ..] str
      where
        parseChar x ' ' = id
        parseChar x ch  = Map.insert (Position2 x y) ch

example :: Grid
example =
  parseGrid
    [ "     |          "
    , "     |  +--+    "
    , "     A  |  C    "
    , " F---|----E|--+ "
    , "     |  |  |  D "
    , "     +B-+  +--+ "
    ]
