module Y2024.Day05
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type PageOrdering = (Int, Int)

type PageOrderings = Set PageOrdering

data Input = Input
  { iOrdering :: PageOrderings
  , iPages    :: [[Int]]
  } deriving (Show)

parser :: Parser Text Input
parser =
  lineGroupsP
    &* ap2P Input (Set.fromList <$> traverseP orderingP) (traverseP pagesP)
  where
    orderingP = tsplitP "|" &* (integerP &+ integerP)
    pagesP = tsplitP "," &** integerP

inRightOrder :: PageOrderings -> [Int] -> Bool
inRightOrder _ [] = True
inRightOrder os (p1:ps) =
  all (\p2 -> Set.notMember (p2, p1) os) ps && inRightOrder os ps

checkFullOrder :: PageOrderings -> [[Int]] -> [[Int]]
checkFullOrder os pps =
  either terror (const pps)
    $ sequence
    $ do
        ps <- pps
        (p1, ps') <- picks ps
        p2 <- ps'
        if Set.notMember (p1, p2) os && Set.notMember (p2, p1) os
          then pure $ Left $ tshow (p1, p2, "not in list")
          else []

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

sumMiddles :: Num a => [[a]] -> a
sumMiddles = sum . map middle

part1 :: Input -> Int
part1 Input {..} =
  sumMiddles $ filter (inRightOrder iOrdering) $ checkFullOrder iOrdering iPages

pageOrdering :: PageOrderings -> Int -> Int -> Ordering
pageOrdering os p1 p2
  | p1 == p2 = EQ
  | Set.member (p1, p2) os = LT
  | Set.member (p2, p1) os = GT
  | otherwise = error $ "No ordering: " ++ show (p1, p2)

part2 :: Input -> Int
part2 Input {..} =
  sumMiddles
    $ map (sortBy (pageOrdering iOrdering))
    $ filter (not . inRightOrder iOrdering)
    $ checkFullOrder iOrdering iPages

tasks =
  Tasks
    (AOC 2024 5)
    (CodeBlock 0)
    parser
    [task part1 143 & taskPart 1, task part2 123 & taskPart 2]
