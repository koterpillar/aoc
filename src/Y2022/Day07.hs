module Y2022.Day07 where

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

data Command
  = CD Text
  | LS FileDir
  deriving (Eq, Show)

type Input = [Command]

cmdHeaderP :: Parser Text Command
cmdHeaderP =
  wordsP &*
  ((requireP ["ls"] &* constP (LS emptyDir)) &|
   pairPWith (const CD) (requireP "cd") idP)

cmdBodyP :: Command -> Parser [Text] Command
cmdBodyP c@CD {} = requireP [] &* constP c
cmdBodyP (LS _)  = LS . Dir . Map.fromList <$> traverseP lsItemP

lsItemP :: Parser Text (Text, FileDir)
lsItemP =
  wordsP &*
  (((requireP "dir" &* constP emptyDir) &| (integerP &* pureP File)) &+ idP) &*
  pureP swap

cmdP :: Parser [Text] Command
cmdP = unconsP &* (cmdHeaderP &=> cmdBodyP)

parser :: Parser Text Input
parser = tsplitP "$ " &* pureP (filter $ not . Text.null) &** linesP &* cmdP

mkFileTree :: Input -> FileDir
mkFileTree = snd . foldl' go (undefined, emptyDir)

type Session = ([Text], FileDir)

go :: Session -> Command -> Session
go (_, dir) (CD "/")      = ([], dir)
go (path, dir) (CD "..")  = (tail path, dir)
go (path, dir) (CD name)  = (name : path, dir)
go (path, dir) (LS files) = (path, go' (reverse path) files dir)

go' :: [Text] -> FileDir -> FileDir -> FileDir
go' [] files _ = files
go' (name:path) files (Dir t) =
  Dir $ Map.alter (Just . go' path files . fromMaybe emptyDir) name t

part1 :: Input -> Int
part1 = error . show . mkFileTree

tasks = Tasks 2022 7 (CodeBlock 1) parser [Task part1 95437]
