{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day11 where

import           Control.Monad.State.Strict
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Utils

data MonkeyFn
  = Square
  | Plus Int
  | Mult Int
  deriving (Ord, Eq, Show)

divisor :: Int
divisor = product $ setFromList [2, 3, 5, 7, 11, 13, 17, 19, 23]

normalize :: Item -> Item
normalize (Item x) = Item (x `mod` divisor)

newtype Item =
  Item Int
  deriving (Show)

mkItem :: Int -> Item
mkItem = Item

itemApply :: MonkeyFn -> Item -> Item
itemApply Square (Item x)   = normalize $ Item (x * x)
itemApply (Plus n) (Item x) = normalize $ Item (x + n)
itemApply (Mult n) (Item x) = normalize $ Item (x * n)

itemDeworry :: Integer -> Item -> Item
itemDeworry worryDiv (Item x) = Item (x `div` fromInteger worryDiv)

itemTest :: Int -> Item -> Bool
itemTest testDivisor i@(Item x) = x `mod` testDivisor == 0

data Monkey =
  Monkey
    { _mItems       :: [Item]
    , _mOp          :: MonkeyFn
    , _mTestDivisor :: Int
    , _mTrue        :: Int
    , _mFalse       :: Int
    , _mSeen        :: Int
    }
  deriving (Show)

makeLenses ''Monkey

monkeyP :: Parser [Text] Monkey
monkeyP = ap5P mk itemsP fnP lastNumP lastNumP lastNumP
  where
    mk _mItems _mOp _mTestDivisor _mTrue _mFalse = Monkey {..}
      where
        _mSeen = 0
    itemsP =
      pureP (terase "  Starting items: ") &* tsplitP "," &**
      (mkItem <$> integerP)
    fnP =
      pureP (terase "  Operation: new = old ") &* wordsP &* pairP &*
      tupleBindP fnP1
    fnP1 "*" = (Mult <$> integerP) &| (Square <$ requireP "old")
    fnP1 "+" = Plus <$> integerP

lastNumP :: Parser Text Int
lastNumP = wordsP &* pureP last &* integerP

type Situation = Map Int Monkey

parser :: Parser Text Situation
parser =
  mapFromList <$> lineGroupsP &** unconsP &* (pureP (terase ":") &* lastNumP) &=
  monkeyP

traceItems :: State Situation ()
traceItems = do
  st <- get
  let items = Map.map _mItems st
  traceM $ "Result: " ++ show items

traceSeen :: State Situation ()
traceSeen = do
  st <- get
  for_ (Map.toList $ Map.map _mSeen st) $ \(i, seen) ->
    traceM $ "Monkey " ++ show i ++ " inspected items " ++ show seen ++
    " times."

move :: Integer -> State Situation ()
move worryDiv = do
  ms <- gets Map.keys
  for_ ms $ monkeyMove worryDiv

monkeyMove :: Integer -> Int -> State Situation ()
monkeyMove worryDiv mi = do
  mm@Monkey {..} <- gets $ mapLookupE "monkeyMove" mi
  for_ _mItems $ inspect worryDiv mi mm

mcount :: Int -> State Situation ()
mcount mi = ix mi . mSeen += 1

inspect :: Integer -> Int -> Monkey -> Item -> State Situation ()
inspect worryDiv mi Monkey {..} item = do
  let i1 = itemApply _mOp item
  let i2 = itemDeworry worryDiv i1
  let target =
        if itemTest _mTestDivisor i2
          then _mTrue
          else _mFalse
  mcount mi
  throwTo mi target i2

throwTo :: Int -> Int -> Item -> State Situation ()
throwTo from to item = do
  ix to . mItems %= (++ [item])
  ix from . mItems %= tail

mbusiness :: Situation -> Int
mbusiness st = product $ take 2 $ reverse $ sort $ map _mSeen $ toList st

moves :: Integer -> Int -> State Situation ()
moves worryDiv rounds =
  for_ [1 .. rounds] $ \r -> do
    move worryDiv
    when (r `mod` 100 == 0 || r == 1 || r == 20) $ traceM $ "Round " ++ show r
    when (r `mod` 1000 == 0 || r == 1 || r == 20) traceSeen

part :: Integer -> Int -> Situation -> Int
part worryDiv rounds = mbusiness . execState (moves worryDiv rounds)

part1 :: Situation -> Int
part1 = part 3 20

part2 :: Situation -> Int
part2 = part 1 10000

tasks =
  Tasks 2022 11 (CodeBlock 0) parser [Task part1 10605, Task part2 2713310158]
