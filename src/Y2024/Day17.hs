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

type Opcode = Int

type Program = [Opcode]

data Machine = Machine
  { mRegisters :: Registers
  , mProgram   :: Program
  , mPC        :: Int
  , mOutputRev :: [Int]
  } deriving (Ord, Eq, Show)

mOutput :: Machine -> [Int]
mOutput = reverse . mOutputRev

mInstructions :: Machine -> [Opcode]
mInstructions m = drop (mPC m) $ mProgram m

mInstruction :: Machine -> Maybe Opcode
mInstruction = listToMaybe . mInstructions

mLiteralOperand :: Machine -> Int
mLiteralOperand = (!! 1) . mInstructions

mComboOperand :: Machine -> Int
mComboOperand m =
  case mLiteralOperand m of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> mRegister 'A' m
    5 -> mRegister 'B' m
    6 -> mRegister 'C' m
    o -> error $ "invalid combo operand" <> show o

mRegister :: Register -> Machine -> Int
mRegister r = mapLookupE "mRegister" r . mRegisters

mSetRegister :: Register -> Int -> Machine -> Machine
mSetRegister r v m = m {mRegisters = Map.insert r v $ mRegisters m}

mWrite :: Int -> Machine -> Machine
mWrite o m = m {mOutputRev = o : mOutputRev m}

parser :: Parser Text Machine
parser =
  (\(r, p) -> Machine r p 0 [])
    <$> tsplitP "\n\n"
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

mDiv :: Register -> Machine -> Machine
mDiv r m = mSetRegister r v m
  where
    v = mRegister 'A' m `shiftR` mComboOperand m

mOut :: Machine -> Machine
mOut m = mWrite v m
  where
    v = mComboOperand m `mod` 8

mJnz :: Machine -> Machine
mJnz m
  | mRegister 'A' m == 0 = m
  | otherwise = m {mPC = mLiteralOperand m - 2} -- -2 because of mAdvance

mBst :: Machine -> Machine
mBst m = mSetRegister 'B' v m
  where
    v = mComboOperand m `mod` 8

mBxl :: Machine -> Machine
mBxl m = mSetRegister 'B' v m
  where
    v = mLiteralOperand m `xor` mRegister 'B' m

mBxc :: Machine -> Machine
mBxc m = mSetRegister 'B' v m
  where
    v = mRegister 'C' m `xor` mRegister 'B' m

mStep :: Machine -> Maybe Machine
mStep m =
  case mInstruction (traceShowId m) of
    Nothing -> Nothing
    Just i  -> Just $ mAdvance $ go i m
  where
    go 0 = mDiv 'A'
    go 1 = mBxl
    go 2 = mBst
    go 3 = mJnz
    go 4 = mBxc
    go 5 = mOut
    go 6 = mDiv 'B'
    go 7 = mDiv 'C'
    go i = error $ "invalid instruction " <> show i

mRun :: Machine -> [Int]
mRun = mOutput . iterateMaybeL mStep

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
