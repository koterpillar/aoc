import Data.List.Utils

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Graph
import Utils

type Program = Int

type Pipes = Map Program (Set Program)

readPipes :: IO Pipes
readPipes = parsePipes <$> readLines

parsePipes :: [String] -> Pipes
parsePipes =
  Map.fromListWith Set.union . concatMap (readPipeLine . words . replace "," "")
  where
    readPipeLine :: [String] -> [(Program, Set Program)]
    readPipeLine (p':_:ps') =
      let p = read p'
          ps = map read ps'
      in [(p, Set.singleton p2) | p2 <- ps] ++
         [(p2, Set.singleton p) | p2 <- ps]
