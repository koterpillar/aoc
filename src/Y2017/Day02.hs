module Y2017.Day02 where

import           AOC
import           Utils

lineChecksum :: [Int] -> Int
lineChecksum lst = maximum lst - minimum lst

checksum :: [[Int]] -> Int
checksum = sum . map lineChecksum

divideDivisible :: [Int] -> Int
divideDivisible lst =
  fromJustE "divideDivisible"
    $ findTuple (== 0)
    $ map (uncurry divMod)
    $ filter (uncurry (/=))
    $ liftA2 (,) lst lst

tasks =
  Tasks
    2017
    2
    (CodeBlock 0)
    (linesP &** integersSpaceP)
    [ task checksum 18
    , task (sum . map divideDivisible) 9 & taskScraper (CodeBlock 1)
    ]
