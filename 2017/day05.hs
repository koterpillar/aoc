module Day05 where

import qualified Data.Map.Strict as M

import Utils

type Address = Int

type Offset = Int

type Program = M.Map Address Offset

data Computer = Computer
  { cIP :: !Address
  , cProgram :: !Program
  , cTime :: !Int
  } deriving (Show)

step' :: (Int -> Int) -> Computer -> Either Int Computer
step' changeOffset (Computer idx instrs time) =
  case M.lookup idx instrs of
    Just offset -> Right (Computer (idx + offset) (M.insert idx (changeOffset offset) instrs) (time + 1))
    Nothing -> Left time

step = step' (+ 1)

step2 = step' $ \o -> if o >= 3 then o - 1 else o + 1

readProgram :: IO Program
readProgram = go <$> readLines
  where
    go = M.fromList . zip [0 ..] . map read

testProgram :: Program
testProgram = M.fromList $ zip [0 ..] [0, 3, 0, 1, -3]

mkComputer :: Program -> Computer
mkComputer prg = Computer 0 prg 0

tillExit :: (a -> Either b a) -> a -> b
tillExit f = go
  where
    go x =
      case f x of
        Right x' -> go x'
        Left r -> r
