module Day06 where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Utils

type Bank = Int

type Blocks = Int

type Banks = M.Map Bank Blocks

redistribute :: Banks -> Banks
redistribute banks = banks'''
  where
    maxfill = maximum banks
    cnt = length banks
    maxIndex = fst $ head $ filter (\(i, b) -> b == maxfill) $ M.toList banks
    (uniform, leftover) = maxfill `divMod` cnt
    banks' = M.insert maxIndex 0 banks
    banks'' = M.map (+ uniform) banks'
    leftovers =
      M.fromList $
      zip (map (`mod` cnt) [maxIndex + 1 ..]) (replicate leftover 1)
    banks''' = M.unionWith (+) banks'' leftovers

mkBanks :: [Int] -> Banks
mkBanks contents = M.fromList $ zip [0 ..] contents

exampleBanks = mkBanks [0, 2, 7, 0]

findLoop fn x = go 0 x M.empty
  where
    go i x seen =
      case M.lookup x seen of
        Just oi -> (i, i - oi)
        Nothing -> go (i + 1) (fn x) (M.insert x i seen)
