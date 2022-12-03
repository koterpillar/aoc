module Y2020.Day14 where

import           Control.Monad (zipWithM)

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
    parseSetMask = SetMask . snd <$> requireP "mask" &= parseBitMask
    parseSetMem =
      uncurry SetMem <$>
      (pureP (terase "mem[" . terase "]") &* integerP) &= integerP

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

applyMask2 :: BitsMask -> Int -> [Int]
applyMask2 (BitsMask m) = map bitsValue . zipWithM amb m . toBitStringPad
  where
    amb (Set O) b = [b]
    amb (Set I) b = [I]
    amb Ignore _  = [O, I]

applyInstruction2 :: CPU -> Instruction -> CPU
applyInstruction2 (CPU _ mem) (SetMask mask) = CPU mask mem
applyInstruction2 (CPU mask mem) (SetMem addr val) =
  CPU mask $
  flip mappend mem $ mapFromList [(addr', val) | addr' <- applyMask2 mask addr]

part2 = sum . cMem . foldl' applyInstruction2 (CPU undefined mempty)

tasks =
  Tasks
    2020
    14
    (CodeBlock 0)
    (linesP &** parseInstruction)
    [ Task part1 165
    , Assert
        "amb"
        [26, 27, 58, 59]
        (applyMask2
           (justParse parseBitMask "000000000000000000000000000000X1001X")
           42)
    , TaskScraper (CodeBlock 4) part2 208
    ]
