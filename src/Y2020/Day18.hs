module Y2020.Day18 where

import           Control.Monad.State

import           AOC
import           Utils

data Op
  = OPlus
  | OMult
  deriving (Eq, Show)

data Token
  = TNum Int
  | TOp Op
  | TLParen
  | TRParen
  deriving (Eq, Show)

tokenP :: Parser Text Token
tokenP =
  (TNum <$> integerP) &|
  choiceP [("+", TOp OPlus), ("*", TOp OMult), ("(", TLParen), (")", TRParen)]

parseExpr :: Parser Text [Token]
parseExpr = pureP (treplace "(" "( " . treplace ")" " )") &* wordsP &** tokenP

type SP a = StateParser [Token] a

runSP = justParse . stateP

arg :: Priority -> SP Int
arg prio =
  unconsSP_ >>= \case
    TLParen -> evaluate prio <* ensureUnconsSP_ TRParen
    TNum v  -> pure v
    a       -> failSP $ "arg: " ++ show a

opApply :: Op -> Int -> Int -> Int
opApply OPlus = (+)
opApply OMult = (*)

type Priority = Op -> Int

priority1 :: Priority
priority1 = const 1

priority2 :: Priority
priority2 OPlus = 2
priority2 OMult = 1

evaluate :: Priority -> SP Int
evaluate prio = go []
  where
    go stk = do
      a <- arg prio
      unconsSP >>= \case
        Nothing       -> pure $ unwindAll stk a
        Just TRParen  -> putBackSP TRParen *> pure (unwindAll stk a)
        Just (TOp op) -> go $ unwind prio stk a op
        Just a        -> failSP $ "evaluate: " ++ show a

unwindAll :: [(Int, Op)] -> Int -> Int
unwindAll [] a            = a
unwindAll ((a, o):rest) b = unwindAll rest $ opApply o a b

unwind :: Priority -> [(Int, Op)] -> Int -> Op -> [(Int, Op)]
unwind _ [] a o = [(a, o)]
unwind prio cur@((a, o1):rest) b o2
  | prio o2 > prio o1 = (b, o2) : cur
  | otherwise = unwind prio rest (opApply o1 a b) o2

part1 = sum . map (runSP $ evaluate priority1)

part2 = sum . map (runSP $ evaluate priority2)

normalPriority :: Priority
normalPriority OPlus = 1
normalPriority OMult = 2

normalEvaluate = evaluate normalPriority

tasks =
  Tasks
    2020
    18
    (InlineCode 4)
    (linesP &** parseExpr)
    [ Assert
        "normalEvaluate"
        (1 * 2 + 3 * 4)
        (runSP normalEvaluate $ justParse parseExpr "1 * 2 + 3 * 4")
    , Task part1 12240
    , Task part2 669060
    ]
