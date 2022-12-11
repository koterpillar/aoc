{-# LANGUAGE Strict #-}

module Y2022.Day11 where

import           Control.Monad.State.Strict
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Utils

data MonkeyFn
  = Square
  | Plus Integer
  | Mult Integer
  deriving (Ord, Eq, Show)

mfnApply :: MonkeyFn -> Integer -> Integer
mfnApply Square x   = x * x
mfnApply (Plus n) x = x + n
mfnApply (Mult n) x = x * n

data Monkey =
  Monkey
    { mItems       :: [Integer]
    , mOp          :: MonkeyFn
    , mTestDivisor :: Integer
    , mTrue        :: Int
    , mFalse       :: Int
    , mSeen        :: Integer
    }
  deriving (Ord, Eq, Show)

monkeyP :: Parser [Text] Monkey
monkeyP =
  mk <$>
  unconsP &* itemsP &=
  (unconsP &* fnP &= (unconsP &* lastNumPi &= (lastNumP &+ lastNumP)))
  where
    mk (mItems, (mOp, (mTestDivisor, (mTrue, mFalse)))) = Monkey {..}
      where
        mSeen = 0
    itemsP =
      pureP (terase "  Starting items: ") &* tsplitP "," &**
      (toInteger <$> integerP)
    fnP =
      pureP (terase "  Operation: new = old ") &* wordsP &* pairP &*
      tupleBindP fnP1
    fnP1 "*" = (Mult . toInteger <$> integerP) &| (Square <$ requireP "old")
    fnP1 "+" = Plus . toInteger <$> integerP

lastNumP :: Parser Text Int
lastNumP = wordsP &* pureP last &* integerP

lastNumPi :: Parser Text Integer
lastNumPi = toInteger <$> lastNumP

type Situation = Map Int Monkey

parser :: Parser Text Situation
parser =
  mapFromList <$>
  lineGroupsP &** unconsP &* (pureP (terase ":") &* lastNumP) &= monkeyP

traceItems :: State Situation ()
traceItems = do
  st <- get
  let items = Map.map mItems st
  traceM $ "Result: " ++ show items

move :: Integer -> State Situation ()
move worryDiv = do
  ms <- gets Map.keys
  for_ ms $ monkeyMove worryDiv

monkeyMove :: Integer -> Int -> State Situation ()
monkeyMove worryDiv mi = do
  mm@Monkey {..} <- gets $ mapLookupE "monkeyMove" mi
  for_ mItems $ inspect worryDiv mi mm

mcount :: Int -> State Situation ()
mcount mi = mmodify mi $ \m -> m {mSeen = succ (mSeen m)}

mmodify :: Int -> (Monkey -> Monkey) -> State Situation ()
mmodify mi f = modify $ Map.adjust f mi

inspect :: Integer -> Int -> Monkey -> Integer -> State Situation ()
inspect worryDiv mi Monkey {..} item = do
  let i1 = mfnApply mOp item
  let i2 = i1 `div` worryDiv
  let target =
        if i2 `mod` mTestDivisor == 0
          then mTrue
          else mFalse
  mcount mi
  throwTo mi target i2

throwTo :: Int -> Int -> Integer -> State Situation ()
throwTo from to item = do
  mmodify to $ \m -> m {mItems = mItems m ++ [item]}
  mmodify from $ \m -> m {mItems = tail (mItems m)}

mbusiness :: Situation -> Integer
mbusiness st = product $ take 2 $ reverse $ sort $ map mSeen $ toList st

moves :: Integer -> Int -> State Situation ()
moves worryDiv rounds = for_ [1 .. rounds] $ \r -> move worryDiv

part :: Integer -> Int -> Situation -> Integer
part worryDiv rounds = mbusiness . execState (moves worryDiv rounds)

part1 :: Situation -> Integer
part1 = part 3 20

part2 :: Situation -> Integer
part2 = part 1 10000

tasks =
  Tasks 2022 11 (CodeBlock 0) parser [Task part1 10605, Task part2 2713310158]
