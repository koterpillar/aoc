module Y2024.Day09 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type FileID = Int

type FilePos = Int

type Filesystem = Map FilePos FileID

mkFS :: [Int] -> Filesystem
mkFS = fmap fromJust . Map.filter isJust . Map.fromList . zip [0 ..] . go 0
  where
    go :: Int -> [Int] -> [Maybe FileID]
    go fileID [fileSize] = replicate fileSize $ Just fileID
    go fileID (fileSize:emptySize:xs) =
      replicate fileSize (Just fileID)
        ++ replicate emptySize Nothing
        ++ go (fileID + 1) xs

parser :: Parser Text Filesystem
parser = pureP Text.stripEnd &* digitsP &* pureP mkFS

fsMax :: Filesystem -> FilePos
fsMax fs = m
  where
    (m, _) = Map.findMax fs

compact :: Filesystem -> Filesystem
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

checksum :: Filesystem -> Int
checksum = sum . map (uncurry (*)) . Map.toList

displayFS :: Filesystem -> Text
displayFS fs =
  Text.pack $ do
    pos <- [0 .. fsMax fs]
    pure $ maybe '.' (head . show) $ Map.lookup pos fs

part1 :: Filesystem -> Int
part1 = checksum . compact . ttraceF displayFS

tasks = Tasks 2024 9 (CodeBlock 0) parser [task part1 1928 & taskPart 1]
