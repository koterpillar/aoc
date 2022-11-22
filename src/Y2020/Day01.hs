module Y2020.Day01 where

import qualified Data.Set as Set

import           AOC
import           Path
import           Utils

findSum :: Int -> Int -> [Int] -> Set Int
findSum count total candidates =
  lastE "empty path" $
  fromJustE "no numbers add to requested" $
  aStar prependNumber distance toGoal isGoal Set.empty
  where
    prependNumber :: Set Int -> HashSet (Set Int)
    prependNumber existing =
      hashSetFromList
        [Set.insert x existing | x <- candidates, x `Set.notMember` existing]
    distance = const $ const 1
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
    01
    (CodeBlock 0)
    (linesP &** integerP)
    [ Task (product . findSum 2 2020) 514579
    , Task (product . findSum 3 2020) 241861950
    ]
