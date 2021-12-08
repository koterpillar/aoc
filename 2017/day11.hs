import Data.Char

import Data.List.Split

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

data Dir6
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

all6 :: [Dir6]
all6 = [minBound .. maxBound]

opp6 :: Dir6 -> Dir6
opp6 = toEnum . (`mod` 6) . (+ 3) . fromEnum

next6 :: Dir6 -> Dir6
next6 = toEnum . (`mod` 6) . succ . fromEnum

readDirs :: String -> [Dir6]
readDirs = map (read . map toUpper) . splitOn ","

type Path = Map Dir6 Int

pGet :: Dir6 -> Path -> Int
pGet dir = fromMaybe 0 . Map.lookup dir

pSet :: Dir6 -> Int -> Path -> Path
pSet dir 0 = Map.delete dir
pSet dir i = Map.insert dir i

optDirs :: [Dir6] -> [Dir6]
optDirs = concatMap replTuple . Map.toList . go . mkPath
  where
    go :: Path -> Path
    go = pOptTriangle . pOptOpposite . pOptTriangle

mkPath :: [Dir6] -> Path
mkPath = Map.fromListWith (+) . flip zip (repeat 1)

pOptOpposite :: Path -> Path
pOptOpposite path = foldr cancelOut path all6

cancelOut :: Dir6 -> Path -> Path
cancelOut dir path =
  let opp = opp6 dir
      there = pGet dir path
      back = pGet opp path
  in if there >= back
       then pSet opp 0 $ pSet dir (there - back) path
       else path

pOptTriangle :: Path -> Path
pOptTriangle path = foldr cancelTriangle path all6

cancelTriangle :: Dir6 -> Path -> Path
cancelTriangle dir1 path =
  let dirR = next6 dir1
      dir2 = next6 dirR
      x1 = pGet dir1 path
      x2 = pGet dir2 path
      xR = pGet dirR path
      xmin = min x1 x2
  in pSet dirR (xR + xmin) $ pSet dir1 (x1 - xmin) $ pSet dir2 (x2 - xmin) path

replTuple :: (a, Int) -> [a]
replTuple (a, n) = replicate n a
