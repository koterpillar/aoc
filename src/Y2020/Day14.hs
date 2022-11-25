module Y2020.Day14 where

import           Data.Bits

import           AOC
import           Bit
import           Utils

data BitMask
  = Set Bit
  | Ignore
  deriving (Eq, Show)

showBitMask :: BitMask -> Char
showBitMask (Set b) = head $ show b
showBitMask Ignore  = 'X'

newtype BitsMask =
  BitsMask [BitMask]

instance Show BitsMask where
  show (BitsMask mask) = map showBitMask mask

parseBitMask :: Parser Text BitsMask
parseBitMask =
  BitsMask <$>
  charactersP &** choiceP [('0', Set O), ('1', Set I), ('X', Ignore)]

data Instruction
  = SetMask BitsMask
  | SetMem Int Int
  deriving (Show)

parseInstruction :: Parser Text Instruction
parseInstruction = tsplitP " = " &* pairP &* (parseSetMask &| parseSetMem)
  where
    parseSetMask = SetMask . snd <$> (requireP "mask" &= parseBitMask)
    parseSetMem =
      uncurry SetMem <$>
      (pureP (treplace "mem[" "" . treplace "]" "") &* integerP &= integerP)

data CPU =
  CPU
    { cMask :: BitsMask
    , cMem  :: Map Int Int
    }
  deriving (Show)

toBitStringPad = listPad O 36 . toBitString

listPad a n xs = replicate (n - length xs) a ++ xs

applyMask :: BitsMask -> Int -> Int
applyMask (BitsMask m) = bitsValue . zipWith applyMaskBit m . toBitStringPad
  where
    applyMaskBit (Set b) _ = b
    applyMaskBit Ignore b  = b

applyInstruction :: CPU -> Instruction -> CPU
applyInstruction (CPU _ mem) (SetMask mask) = CPU mask mem
applyInstruction (CPU mask mem) (SetMem addr val) =
  CPU mask $ mapInsert addr (applyMask mask val) mem

part1 = sum . cMem . foldl' applyInstruction (CPU undefined mempty)

tasks =
  Tasks 2020 14 (CodeBlock 0) (linesP &** parseInstruction) [Task part1 165]
