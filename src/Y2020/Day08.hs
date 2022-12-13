module Y2020.Day08 where

import           AOC
import           Utils

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Eq, Show)

parseInstruction :: Parser Text Instruction
parseInstruction =
  uncurry ($) <$>
  wordsP &* choiceP [("acc", Acc), ("jmp", Jmp), ("nop", Nop)] &+
  (pureP (terase "+") &* integerP)

type Program = Map Int Instruction

parseProgram :: Parser Text Program
parseProgram = mapFromList . zipN 0 <$> linesP &** parseInstruction

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
markVisited cpu = cpu {cVisited = setInsert (cPC cpu) (cVisited cpu)}

data Error
  = PastTheEnd
  | OutOfBounds
  deriving (Ord, Eq, Show)

execute :: Instruction -> CPU -> CPU
execute (Nop _) cpu = (markVisited cpu) {cPC = cPC cpu + 1}
execute (Acc n) cpu = (markVisited cpu) {cPC = cPC cpu + 1, cAcc = cAcc cpu + n}
execute (Jmp n) cpu = (markVisited cpu) {cPC = cPC cpu + n}

getInstruction :: Program -> CPU -> Instruction
getInstruction program cpu = mapLookupE "instruction" (cPC cpu) program

looped :: CPU -> Bool
looped cpu = setMember (cPC cpu) (cVisited cpu)

isOOB :: Program -> CPU -> Bool
isOOB program CPU {..} = cPC == length program

step :: Program -> CPU -> CPU
step program cpu = execute (getInstruction program cpu) cpu

data Result
  = Loop CPU
  | OOB CPU
  | StrageOOB CPU
  deriving (Eq, Show)

rCPU :: Result -> CPU
rCPU (Loop cpu)      = cpu
rCPU (OOB cpu)       = cpu
rCPU (StrageOOB cpu) = cpu

run :: Program -> CPU -> Result
run program cpu
  | looped cpu = Loop cpu
  | isOOB program cpu = OOB cpu
  | not (mapMember (cPC cpu) program) = StrageOOB cpu
  | otherwise = run program (step program cpu)

findLoop :: Program -> Int
findLoop program =
  let Loop cpu = run program mkCPU
   in cAcc cpu

flipInstruction :: Instruction -> Maybe Instruction
flipInstruction (Nop n) = Just $ Jmp n
flipInstruction (Jmp n) = Just $ Nop n
flipInstruction _       = Nothing

runFlip :: Program -> CPU -> Result
runFlip program cpu
  | looped cpu = Loop cpu
  | isOOB program cpu = OOB cpu
  | not (mapMember (cPC cpu) program) = StrageOOB cpu
  | otherwise = runOneFlip program cpu

runOneFlip :: Program -> CPU -> Result
runOneFlip program cpu =
  case flipped of
    Nothing -> runFlip program (step program cpu)
    Just i' ->
      let cpu' = execute i' cpu
          result' = run program cpu'
       in case result' of
            r@OOB {} -> r
            r        -> runFlip program $ execute instr cpu
  where
    instr = getInstruction program cpu
    flipped = flipInstruction instr

findOOB :: Program -> Int
findOOB program =
  case runFlip program mkCPU of
    OOB cpu -> cAcc cpu
    r       -> error $ "No OOB, got: " ++ show r

tasks =
  Tasks 2020 8 (CodeBlock 0) parseProgram [Task findLoop 5, Task findOOB 8]
