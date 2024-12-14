module Y2023.Day06
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Race = Race
  { rTime     :: Int
  , rDistance :: Int
  } deriving (Show, Eq, Ord)

parser :: Parser Text [Race]
parser =
  uncurry (zipWith Race)
    <$> ((linesP &** wordsP &* pureP tail &** integerP) &* pairP)

waysToWin :: Race -> Int
waysToWin (Race t d) = length $ [hold | hold <- [0 .. t], distance hold t > d]
  where
    distance hold time = hold * (time - hold)

part1 = product . map waysToWin

unkern = read . concatMap show

mergeRaces :: [Race] -> Race
mergeRaces rs = Race (unkern times) (unkern distances)
  where
    times = map rTime rs
    distances = map rDistance rs

part2 = waysToWin . mergeRaces

tasks = Tasks 2023 6 (CodeBlock 0) parser [Task part1 288, Task part2 71503]
