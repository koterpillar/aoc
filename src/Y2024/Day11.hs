module Y2024.Day11 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Memo
import           Utils

parser :: Parser Text [Int]
parser = integersSpaceP

evolve :: Int -> [Int]
evolve 0 = [1]
evolve v =
  let vs = show v
      vsl = length vs
   in if even vsl
        then let (vs1, vs2) = splitAt (vsl `div` 2) vs
              in [read vs1, read vs2]
        else [2024 * v]

evolveN :: Int -> [Int] -> Int
evolveN n vs = stateMemo2 go (\rgo -> mgo rgo n vs)
  where
    go _ 0 _   = pure 1
    go rgo n v = mgo rgo (pred n) (evolve v)
    mgo rgo n vs = sum <$> traverse (rgo n) vs

part1 :: [Int] -> Int
part1 = evolveN 25

part2 :: [Int] -> Int
part2 = evolveN 75

tasks =
  Tasks
    2024
    11
    (Inline "125 17")
    parser
    [task part1 55312 & taskPart 1, taskBlind part2 & taskPart 2]
