import Data.List.Utils

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

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

expand :: Pipes -> Set Program -> Set Program
expand pipes ps = ps <> mconcat (mapMaybe (`Map.lookup` pipes) $ Set.toList ps)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x =
  let x' = f x
  in if x == x'
       then x
       else fixPoint f x'

reachableFrom :: Pipes -> Program -> Set Program
reachableFrom pipes p = fixPoint (expand pipes) (Set.singleton p)

allPrograms :: Pipes -> Set Program
allPrograms = Map.keysSet

unreachableFrom :: Pipes -> Program -> Set Program
unreachableFrom pipes p =
  allPrograms pipes `Set.difference` reachableFrom pipes p

connectedComponents :: Pipes -> [Set Program]
connectedComponents pipes = go (allPrograms pipes)
  where
    go candidates
      | Set.null candidates = []
      | otherwise =
        let candidate = head $ Set.toList candidates
            component = reachableFrom pipes candidate
        in component : go (candidates `Set.difference` component)
