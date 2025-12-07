module Y2025.Day07 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Grid
import Utils

data Item = Src | V deriving (Eq, Ord, Show, Bounded, Enum)

type Grid = Grid2 Item

instance GridItem Item where
    showInGrid Src = 'S'
    showInGrid V = '^'

parser :: Parser Text Grid
parser = charGridP

type St = (Int, Set Position2)

mergeSt :: Int -> [St] -> St
mergeSt existing sts = (existing + sum counts, Set.unions positions)
  where
    (counts, positions) = unzip sts

step1 :: Grid -> St -> St
step1 grid st = mergeSt counts $ map go $ Set.toList positions
  where
    (counts, positions) = st
    go pos =
        let pos' = walk S pos
         in case Map.lookup (walk S pos) grid of
                Just V -> (1, Set.fromList [walk W pos', walk E pos'])
                Nothing -> (0, Set.singleton pos')

finished1 :: Grid -> Set Position2 -> Bool
finished1 grid positions =  stY > maxV
  where
    stY :: Int
    stY = pY $ fromJustE "stY" $ Set.lookupMin positions
    maxV = pY $ maximumOn pY $ Map.keys grid

part1 :: Grid -> Int
part1 g = fst $ go g ist
  where
    ist = (0, Set.singleton src)
    src = fst $ fromJustE "src" $ find (\(_, v) -> v == Src) $ Map.toList g
    go g st | finished1 g (snd st) = st
            | otherwise = go g $ step1 g st

finished2 :: Grid -> Map Position2 Int -> Bool
finished2 grid positions =  stY > maxV
  where
    stY :: Int
    stY = pY $ fst $ fromJustE "stY" $ Map.lookupMin positions
    maxV = pY $ maximumOn pY $ Map.keys grid

type St2 = Map Position2 Int

mergeSt2 :: [St2] -> St2
mergeSt2 = Map.unionsWith (+)

step2 :: Grid -> St2 -> St2
step2 grid st = mergeSt2 $ map go $ Map.toList st
  where
    go (pos, count) =
        let pos' = walk S pos
         in case Map.lookup (walk S pos) grid of
                Just V -> Map.fromList [(walk W pos', count), (walk E pos', count)]
                Nothing -> Map.singleton pos' count

part2 :: Grid -> Int
part2 g = sum $ go g ist
  where
    ist = Map.singleton src 1
    src = fst $ fromJustE "src" $ find (\(_, v) -> v == Src) $ Map.toList g
    go g st | finished2 g st = st
            | otherwise = go g $ step2 g st

tasks =
    Tasks
        2025
        7
        (CodeBlock 0)
        parser
        [ task part1 21 & taskPart 1
        , task part2 40 & taskPart 2
        ]
