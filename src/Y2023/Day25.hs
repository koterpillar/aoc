module Y2023.Day25
  ( tasks
  ) where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text     as Text

import           Data.Ord

import           System.Random

import           AOC
import           Graph
import           Path
import           Utils

parser :: Parser Text (Graph Text)
parser =
  Map.fromList
    <$> (linesP &** tsplitP ": " &* (idP &+ (Set.fromList <$> wordsP)))

deleteEdges :: Ord v => [(v, v)] -> Graph v -> Graph v
deleteEdges es1 = flip (foldr $ uncurry go) es
  where
    es = concatMap (\e -> [e, swap e]) es1
    go v1 v2 =
      Map.mapWithKey
        (\v ->
           if v == v1
             then Set.filter (/= v2)
             else id)

randomVertices :: RandomGen g => Int -> g -> Graph v -> [v]
randomVertices n gen graph =
  take n $ map (vs !!) $ randomRs (0, length vs - 1) gen
  where
    vs = Map.keys graph

graphPath :: (Hashable v, Ord v, Show v) => Graph v -> v -> v -> [v]
graphPath g a b =
  fromJustE ("graphPath " <> show a <> " " <> show b)
    $ aStarDepthGoal
        (Set.toList . fromMaybe Set.empty . flip Map.lookup g)
        (\v ->
           if v == a
             then 0
             else 1)
        b

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a, b) =
  if a < b
    then (a, b)
    else (b, a)

mostCommonEdge :: (Ord v, Hashable v, Show v) => Graph v -> (v, v)
mostCommonEdge g = fst $ maximumOn snd $ Map.toList edgeFrequencies
  where
    seed = 150
    gen = mkStdGen seed
    cnt = 100
    testPaths =
      let (g1, g2) = System.Random.split gen
       in zip (randomVertices cnt g1 g) (randomVertices cnt g2 g)
    edgeFrequencies =
      mapFromListCount
        $ concatMap (map sortTuple . zipTail . uncurry (graphPath g)) testPaths

part1 :: Graph Text -> Int
part1 g = product $ map Set.size $ connectedComponents g3
  where
    g0 = bidirectional g
    e1 = traceShowF ("e1", ) $ mostCommonEdge g0
    g1 = deleteEdges [e1] g0
    e2 = traceShowF ("e2", ) $ mostCommonEdge g1
    g2 = deleteEdges [e2] g1
    e3 = traceShowF ("e3", ) $ mostCommonEdge g2
    g3 = deleteEdges [e3] g2

tasks = Tasks (AOC 2023 25) (CodeBlock 1) parser [task part1 54 & taskPart 1]
