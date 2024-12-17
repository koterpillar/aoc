module Y2024.Day17
  ( tasks
  ) where

import           Data.Bits

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

type Register = Char

type Registers = Map Register Int

data Combo
  = CI Int
  | CR Register
  deriving (Ord, Eq, Show)

data Instr
  = SHR Register Combo
  | BXL Int
  | BST Combo
  | LOOPZ
  | BXC
  | OUT Combo
  deriving (Ord, Eq, Show)

type Program = [Instr]

parseOps :: [Int] -> Program
parseOps [] = []
parseOps [a] = error "odd number of ops"
parseOps (i:o:rest) = go i o : parseOps rest
  where
    go 0 o = SHR 'A' (mkC o)
    go 1 o = BXL o
    go 2 o = BST (mkC o)
    go 3 0 = LOOPZ
    go 3 o = error $ "unexpected loop to " <> show o
    go 4 _ = BXC
    go 5 o = OUT (mkC o)
    go 6 o = SHR 'B' (mkC o)
    go 7 o = SHR 'C' (mkC o)
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

parser :: Parser Text Machine
parser =
  (\(r, p) -> Machine r p 0)
    <$> tsplitP "\n\n"
          &* ((Map.fromList <$> linesP &** registerValueP)
                &+ (pureP (Text.dropWhile (/= ' '))
                      &* fmap parseOps (integersP ",")))
  where
    registerValueP :: Parser Text (Register, Int)
    registerValueP =
      pureP (Text.drop (Text.length "Register "))
        &* tsplitP ": "
        &* (charP &+ integerP)

mAdvance :: Machine -> Machine
mAdvance m = m {mPC = mPC m + 2}

mPerform :: Instr -> Machine -> Maybe (Int, Machine)
mPerform (SHR r o) m =
  mStep
    $ mAdvance
    $ mSetRegister r (mRegister 'A' m `shiftR` mComboOperand o m) m
mPerform (OUT o) m = Just (mComboOperand o m `mod` 8, mAdvance m)
mPerform LOOPZ m
  | mRegister 'A' m == 0 = mStep $ mAdvance m
  | otherwise = mStep $ m {mPC = 0}
mPerform (BST o) m =
  mStep $ mAdvance $ mSetRegister 'B' (mComboOperand o m `mod` 8) m
mPerform (BXL o) m =
  mStep $ mAdvance $ mSetRegister 'B' (o `xor` mRegister 'B' m) m
mPerform BXC m =
  mStep $ mAdvance $ mSetRegister 'B' (mRegister 'C' m `xor` mRegister 'B' m) m

mStep :: Machine -> Maybe (Int, Machine)
mStep m =
  case mInstruction m of
    Nothing -> Nothing
    Just i  -> mPerform i m

mRun :: Machine -> [Int]
mRun = unfoldr mStep

part1 :: Machine -> Text
part1 = commas . mRun

commas :: [Int] -> Text
commas = Text.intercalate "," . map tshow

tasks =
  Tasks
    2024
    17
    (CodeBlock 0)
    parser
    [task part1 "4,6,3,5,6,3,5,2,1,0" & taskPart 1]
