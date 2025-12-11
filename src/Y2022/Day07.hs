module Y2022.Day07
  ( tasks
  ) where

import           Control.Monad.State

import           Data.List.Split

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

data FileDir
  = File
      { fSize :: Int
      }
  | Dir
      { dFiles :: Map Text FileDir
      }
  deriving (Eq, Show)

emptyDir :: FileDir
emptyDir = Dir mempty

dContents :: FileDir -> [FileDir]
dContents (File {}) = []
dContents (Dir c)   = Map.elems c

data Command
  = CD Text
  | LS FileDir
  deriving (Eq, Show)

type Input = [Command]

cmdHeaderP :: Parser Text Command
cmdHeaderP = wordsP &* unconsBindP go
  where
    go "ls" = ap0P (LS emptyDir)
    go "cd" = ap1P CD idP

cmdBodyP :: Command -> Parser [Text] Command
cmdBodyP c@CD {} = requireP [] &* constP c
cmdBodyP (LS _)  = LS . Dir . Map.fromList <$> traverseP lsItemP

lsItemP :: Parser Text (Text, FileDir)
lsItemP =
  wordsP
    &* (((requireP "dir" &* constP emptyDir) &| (integerP &* pureP File)) &+ idP)
    &* pureP swap

cmdP :: Parser [Text] Command
cmdP = unconsP &* (cmdHeaderP &=> cmdBodyP)

parser :: Parser Text Input
parser = tsplitP "$ " &* pureP (filter $ not . Text.null) &** linesP &* cmdP

mkFileTree :: Input -> FileDir
mkFileTree = snd . foldl' parseCmd (undefined, emptyDir)

type Session = ([Text], FileDir)

parseCmd :: Session -> Command -> Session
parseCmd (_, dir) (CD "/")      = ([], dir)
parseCmd (path, dir) (CD "..")  = (tail path, dir)
parseCmd (path, dir) (CD name)  = (name : path, dir)
parseCmd (path, dir) (LS files) = (path, fillDir (reverse path) files dir)

fillDir :: [Text] -> FileDir -> FileDir -> FileDir
fillDir [] files _ = files
fillDir (name:path) files (Dir t) =
  Dir $ Map.alter (Just . fillDir path files . fromMaybe emptyDir) name t

dirSize :: FileDir -> Int
dirSize (File size) = size
dirSize (Dir c)     = sum $ map dirSize $ Map.elems c

findDirs :: (FileDir -> Bool) -> FileDir -> [FileDir]
findDirs p r = execState (go r) []
  where
    nom :: FileDir -> State [FileDir] ()
    nom f = when (p f) $ modify (f :)
    go d = nom d >> traverse_ go (dContents d)

part1 :: Input -> Int
part1 = sum . map dirSize . findDirs isSmall . mkFileTree
  where
    isSmall File {} = False
    isSmall d       = dirSize d <= 100000

part2 :: Input -> Int
part2 input = dmin
  where
    root = mkFileTree input
    totalSpace = 70000000
    usedSpace = dirSize root
    freeNeeded = 30000000
    enough d@Dir {} = totalSpace - (usedSpace - dirSize d) >= freeNeeded
    enough _        = False
    dmin = minimum $ map dirSize $ findDirs enough root

tasks =
  Tasks (AOC 2022 7) (CodeBlock 1) parser [Task part1 95437, Task part2 24933642]
