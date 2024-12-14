module Y2022.Day05
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

type Stack = [Char]

type Stacks = Map Int Stack

data Instruction = Instruction
  { iFrom  :: Int
  , iTo    :: Int
  , iCount :: Int
  } deriving (Eq, Show)

type Input = (Stacks, [Instruction])

parseInstruction :: Parser Text Instruction
parseInstruction =
  wordsP
    &* pureP (filter $ Text.all isDigit)
    &* ap3P mk integerP integerP integerP
  where
    mk c f t = Instruction f t c

stacksP :: Parser [Text] Stacks
stacksP =
  Map.fromList . zipN 1 . map catMaybes . transpose
    <$> pureP init &* traverseP listsP

listsP :: Parser Text [Maybe Char]
listsP =
  pureP (map (toNothing ' ' . headE "listsP" . tail) . chunksOf 4 . Text.unpack)

parser :: Parser Text Input
parser = lineGroupsP &* stacksP &+ traverseP parseInstruction

applyInstruction :: Instruction -> Stacks -> Stacks
applyInstruction i@(Instruction from to count) =
  iterateNL count
    $ execState
    $ do
        ff <- ix from <<%= tail
        let f = headE "move from empty" ff
        ix to %= (f :)

applyInstruction2 :: Instruction -> Stacks -> Stacks
applyInstruction2 i@(Instruction from to count) =
  execState $ do
    ff <- use $ ix from
    let (fs, f1) = splitAt count ff
    ix from .= f1
    ix to %= (fs ++)

solve :: (Instruction -> Stacks -> Stacks) -> Input -> String
solve app (stacks, instructions) =
  map head $ Map.elems $ foldl' (flip app) stacks instructions

tasks =
  Tasks
    2022
    5
    (CodeBlock 0)
    parser
    [Task (solve applyInstruction) "CMZ", Task (solve applyInstruction2) "MCD"]
