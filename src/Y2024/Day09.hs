module Y2024.Day09
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type FileID = Int

type FilePos = Int

type FSMap = Map FileID (Int, Int) -- start, size

type FSLong = Map FilePos FileID

mkFSChunks :: [Int] -> FSMap
mkFSChunks = Map.fromList . goF 0 0
  where
    goF fileID pos (sz:rest) =
      (fileID, (pos, sz)) : goE (succ fileID) (pos + sz) rest
    goE fileID pos (sz:rest) = goF fileID (pos + sz) rest
    goE _ _ []               = []

toLong :: FSMap -> FSLong
toLong = Map.fromList . foldMap go . Map.toList
  where
    go (fileID, (pos, sz)) = [(pos + i, fileID) | i <- [0 .. sz - 1]]

parser :: Parser Text FSMap
parser = pureP Text.stripEnd &* digitsP &* pureP mkFSChunks

fsMax :: FSLong -> FilePos
fsMax fs = m
  where
    (m, _) = Map.findMax fs

compact :: FSLong -> FSLong
compact = go 0
  where
    go p fs
      | p >= fsMax fs = fs
      | otherwise =
        case Map.lookup p fs of
          Just _ -> go (succ p) fs
          Nothing ->
            let maxF = mapLookupE "maxF" (fsMax fs) fs
             in go (succ p) $ Map.insert p maxF $ Map.delete (fsMax fs) fs

checksum :: FSLong -> Int
checksum = sum . map (uncurry (*)) . Map.toList

displayFS :: FSLong -> Text
displayFS fs =
  Text.pack $ do
    pos <- [0 .. fsMax fs]
    pure $ maybe '.' (head . show) $ Map.lookup pos fs

displayFSC :: FSMap -> Text
displayFSC = displayFS . toLong

part1 :: FSMap -> Int
part1 = checksum . compact . toLong

findGap :: Int -> FSMap -> Maybe Int
findGap gap fs = go $ sortOn fpos $ Map.toList fs
  where
    fpos (_, (p, _)) = p
    go [] = Nothing
    go [_] = Nothing
    go (((_, (p1, sz)):rest@((_, (p2, _)):_)))
      | p1 + sz + gap <= p2 = Just $ p1 + sz
      | otherwise = go rest

compact2 :: FSMap -> FSMap
compact2 fs = go maxID fs
  where
    (maxID, _) = fromJustE "maxID" $ Map.lookupMax fs
    go :: FileID -> FSMap -> FSMap
    go 0 fs = fs
    go fileID fs =
      let (p0, sz) = fromJustE "lookup go arg" $ Map.lookup fileID fs
       in case findGap sz fs of
            Just p
              | p < p0 -> go (pred fileID) $ Map.insert fileID (p, sz) fs {-$ ttraceF displayFSC $ traceShow ("moving", fileID, "to", p)-}
            _ -> go (pred fileID) $ traceShow ("no space for", fileID) fs

part2 :: FSMap -> Int
part2 = checksum . toLong . ttraceF displayFSC . compact2

tasks =
  Tasks
    (AOC 2024 9)
    (CodeBlock 0)
    parser
    [task part1 1928 & taskPart 1, task part2 2858 & taskPart 2]
