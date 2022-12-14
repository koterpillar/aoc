module Y2022.Day05 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type Stack = [Char]

type Stacks = Map Int Stack

data Instruction =
  Instruction
    { iFrom  :: Int
    , iTo    :: Int
    , iCount :: Int
    }
  deriving (Eq, Show)

type Input = (Stacks, [Instruction])

parseInstruction :: Parser Text Instruction
parseInstruction =
  wordsP &* pureP (\[_, x, _, y, _, z] -> (x, (y, z))) &*
  (integerP &= (integerP &= integerP)) &*
  pureP makeInstruction
  where
    makeInstruction (c, (f, t)) = Instruction f t c

stacksP :: Parser [Text] Stacks
stacksP =
  Map.fromList . zipN 1 . map catMaybes . transpose <$>
  pureP init &* traverseP listsP

listsP :: Parser Text [Maybe Char]
listsP =
  pureP (map (toNothing ' ' . headE "listsP" . tail) . chunksOf 4 . Text.unpack)

parser :: Parser Text Input
parser = lineGroupsP &* stacksP &+ traverseP parseInstruction

applyInstruction :: Instruction -> Stacks -> Stacks
applyInstruction i@(Instruction from to count) =
  iterateNL count $ \s0 ->
    let ff = s0 Map.! from
        f = headE "move from empty" ff
        f1 = tail ff
        s1 = Map.insert from f1 s0
        s2 = Map.adjust (f :) to s1
     in s2

applyInstruction2 :: Instruction -> Stacks -> Stacks
applyInstruction2 i@(Instruction from to count) s0 =
  let ff = s0 Map.! from
      fs = take count ff
      f1 = drop count ff
      s1 = Map.insert from f1 s0
      s2 = Map.adjust (fs ++) to s1
   in s2

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
