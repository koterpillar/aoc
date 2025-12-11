module Y2024.Day17
  ( tasks
  ) where

import           Data.Bits

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           GHC.Base  (VecElem (Int16ElemRep))
import           Path
import           Utils

type Register = Char

type Registers = Map Register Int

data Combo
  = CI Int
  | CR Register
  deriving (Ord, Eq, Show)

data Instr
  = ASHR Register Combo
  | BXL Int
  | BST Combo
  | BXC
  | OUT Combo
  deriving (Ord, Eq, Show)

type Program = [Instr]

type Tape = [Int]

parseOps :: Tape -> Program
parseOps = parseOps' . verifyLoop
  where
    verifyLoop p =
      if takeEnd 2 p == [3, 0]
        then dropEnd 2 p
        else error "missing loop at the end"

parseOps' :: Tape -> Program
parseOps' [] = []
parseOps' [a] = error "odd number of ops"
parseOps' (i:o:rest) = go i o : parseOps' rest
  where
    go 0 o = ASHR 'A' (mkC o)
    go 1 o = BXL o
    go 2 o = BST (mkC o)
    go 3 o = error $ "unexpected loop to " <> show o
    go 4 _ = BXC
    go 5 o = OUT (mkC o)
    go 6 o = ASHR 'B' (mkC o)
    go 7 o = ASHR 'C' (mkC o)
    go i o = error $ "invalid instruction " <> show i <> " " <> show o
    mkC 0 = CI 0
    mkC 1 = CI 1
    mkC 2 = CI 2
    mkC 3 = CI 3
    mkC 4 = CR 'A'
    mkC 5 = CR 'B'
    mkC 6 = CR 'C'
    mkC c = error $ "invalid combo " <> show c

data Machine = Machine
  { mRegisters :: Registers
  , mProgram   :: Program
  , mPC        :: Int
  } deriving (Ord, Eq, Show)

mInstruction :: Machine -> Maybe Instr
mInstruction m = listToMaybe $ drop (mPC m `div` 2) $ mProgram m

mComboOperand :: Combo -> Machine -> Int
mComboOperand (CI i) _ = i
mComboOperand (CR r) m = mRegister r m

mRegister :: Register -> Machine -> Int
mRegister r = mapLookupE "mRegister" r . mRegisters

mSetRegister :: Register -> Int -> Machine -> Machine
mSetRegister r v m = m {mRegisters = Map.insert r v $ mRegisters m}

mkMachine :: Registers -> Tape -> Machine
mkMachine r t = Machine r (parseOps t) 0

type MachineRepr = (Registers, Tape)

parser :: Parser Text MachineRepr
parser =
  tsplitP "\n\n"
    &* ((Map.fromList <$> linesP &** registerValueP)
          &+ (pureP (Text.dropWhile (/= ' ')) &* integersP ","))
  where
    registerValueP :: Parser Text (Register, Int)
    registerValueP =
      pureP (Text.drop (Text.length "Register "))
        &* tsplitP ": "
        &* (charP &+ integerP)

mAdvance :: Machine -> Machine
mAdvance m = m {mPC = mPC m + 2}

mPerform :: Maybe Instr -> Machine -> Maybe (Int, Machine)
mPerform (Just (ASHR r o)) m =
  mStep
    $ mAdvance
    $ mSetRegister r (mRegister 'A' m `shiftR` mComboOperand o m) m
mPerform (Just (OUT o)) m = Just (mComboOperand o m `mod` 8, mAdvance m)
mPerform Nothing m
  | mRegister 'A' m == 0 = mStep $ mAdvance m
  | otherwise = mStep $ m {mPC = 0}
mPerform (Just (BST o)) m =
  mStep $ mAdvance $ mSetRegister 'B' (mComboOperand o m `mod` 8) m
mPerform (Just (BXL o)) m =
  mStep $ mAdvance $ mSetRegister 'B' (o `xor` mRegister 'B' m) m
mPerform (Just BXC) m =
  mStep $ mAdvance $ mSetRegister 'B' (mRegister 'C' m `xor` mRegister 'B' m) m

mStep :: Machine -> Maybe (Int, Machine)
mStep m =
  case mInstruction m of
    Just (ASHR r o) ->
      mStep
        $ mAdvance
        $ mSetRegister r (mRegister 'A' m `shiftR` mComboOperand o m) m
    (Just (OUT o)) -> Just (mComboOperand o m `mod` 8, mAdvance m)
    Nothing
      | mRegister 'A' m == 0 -> Nothing
      | otherwise -> mStep $ m {mPC = 0}
    (Just (BST o)) ->
      mStep $ mAdvance $ mSetRegister 'B' (mComboOperand o m `mod` 8) m
    (Just (BXL o)) ->
      mStep $ mAdvance $ mSetRegister 'B' (o `xor` mRegister 'B' m) m
    (Just BXC) ->
      mStep
        $ mAdvance
        $ mSetRegister 'B' (mRegister 'C' m `xor` mRegister 'B' m) m

mRun :: Machine -> [Int]
mRun = unfoldr mStep

part1 :: MachineRepr -> Text
part1 = commas . mRun . traceShowId . uncurry mkMachine
  where
    commas = Text.intercalate "," . map tshow

findABits :: Machine -> Int
findABits = fromSingleE "findABits: ASHR A" . mapMaybe go . mProgram
  where
    go (ASHR 'A' (CI bits)) = Just bits
    go _                    = Nothing

findStartA :: Tape -> Machine -> Int
findStartA t m = last $ fromJustE "findStartA" $ aStarDepthGoal moves distance 0
  where
    moves :: Int -> [Int]
    moves n = [n `shiftL` step + i | i <- [0 .. 1 `shiftL` step - 1]]
    lt = length t
    distance :: Int -> Int
    distance n
      | length r > lt = 100000
      | otherwise = countIf id (zipWith (/=) r' t)
      where
        r = mRun $ m {mRegisters = Map.insert 'A' n $ mRegisters m}
        r' = replicate (lt - length r) 777 ++ r
    step = findABits m

part2 :: MachineRepr -> Int
part2 (r, t) = traceShow t $ traceShow m $ findStartA t m
  where
    m = mkMachine r t

tasks =
  Tasks
    (AOC 2024 17)
    (CodeBlock 0)
    parser
    [ task part1 "4,6,3,5,6,3,5,2,1,0" & taskPart 1
    , task part2 117440 & taskScraper (CodeBlock 1) & taskPart 2
    ]
