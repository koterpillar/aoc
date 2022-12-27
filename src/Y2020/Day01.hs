module Y2020.Day01 where

import           AOC
import           Path
import           Utils

findSum :: Int -> Int -> [Int] -> Set Int
findSum count total candidates =
  lastE "empty path" $
  fromJustE "no numbers add to requested" $
  aStarDepth prependNumber toGoal isGoal mempty
  where
    prependNumber :: Set Int -> [Set Int]
    prependNumber existing =
      [setInsert x existing | x <- candidates, not (setMember x existing)]
    toGoal :: Set Int -> Int
    toGoal numbers
      | length numbers > count = tooFarAway
      | otherwise =
        let got = sum numbers
         in if got > total
              then tooFarAway
              else total - got
    isGoal picked = sum picked == total && length picked == count
    tooFarAway = 1000000000

tasks =
  Tasks
    2020
    1
    (CodeBlock 0)
    (linesP &** integerP)
    [ task (product . findSum 2 2020) 514579
    , task (product . findSum 3 2020) 241861950
    ]
