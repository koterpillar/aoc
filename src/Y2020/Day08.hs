module Y2020.Day08 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Instruction
  = Acc Int
  | Jmp Int
  | Nop
  deriving (Eq, Show)

parseInstruction :: Parser Text Instruction
parseInstruction =
  uncurry ($) <$>
  wordsP &*
  (choiceP [("acc", Acc), ("jmp", Jmp), ("nop", const Nop)] &+
   pureP (Text.replace "+" "") &*
   integerP)

type Program = Map Int Instruction

parseProgram :: Parser Text Program
parseProgram = Map.fromList . zip [0 ..] <$> linesP &** parseInstruction

data CPU =
  CPU
    { cPC      :: Int
    , cAcc     :: Int
    , cVisited :: Set Int
    }
  deriving (Eq, Show)

mkCPU :: CPU
mkCPU = CPU {cPC = 0, cAcc = 0, cVisited = mempty}

markVisited :: CPU -> CPU
markVisited cpu = cpu {cVisited = Set.insert (cPC cpu) (cVisited cpu)}

step :: Program -> CPU -> CPU
step program cpu =
  case Map.lookup (cPC cpu) program of
    Nothing      -> error $ "No instruction at address " ++ show (cPC cpu)
    Just Nop     -> (markVisited cpu) {cPC = cPC cpu + 1}
    Just (Acc n) -> (markVisited cpu) {cPC = cPC cpu + 1, cAcc = cAcc cpu + n}
    Just (Jmp n) -> (markVisited cpu) {cPC = cPC cpu + n}

findAccumBeforeLoop :: Program -> Int
findAccumBeforeLoop program =
  cAcc $
  until (\cpu -> Set.member (cPC cpu) (cVisited cpu)) (step program) mkCPU

tasks = Tasks 2020 8 (CodeBlock 0) parseProgram [Task findAccumBeforeLoop 5]
