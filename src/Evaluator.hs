module Evaluator
  ( Token(..)
  , tokenP
  , expressionP
  , Priority
  , OpApply
  , evaluate
  ) where

import           Data.String (IsString)

import           Data.Text   (Text)
import qualified Data.Text   as Text

import           Miniparse

data Token op arg
  = TConst arg
  | TOp op
  | TLParen
  | TRParen
  deriving (Eq, Show)

tokenP ::
     (Ord src, Show src, Bounded op, Enum op, IsString src)
  => [src]
  -> Parser src arg
  -> Parser src (Token op arg)
tokenP opChoices argP =
  (TConst <$> argP) &| (TOp <$> choiceEBP opChoices) &|
  choiceP [("(", TLParen), (")", TRParen)]

expressionP ::
     (Bounded op, Enum op)
  => [Text]
  -> Parser Text arg
  -> Parser Text [Token op arg]
expressionP opChoices argP =
  pureP (Text.replace "(" " ( " . Text.replace ")" " ) ") &* wordsP &**
  tokenP opChoices argP

type Priority op = op -> Int

type OpApply op arg = op -> arg -> arg -> arg

evaluate ::
     (Show op, Show arg, Eq op, Eq arg)
  => Priority op
  -> OpApply op arg
  -> [Token op arg]
  -> arg
evaluate prio opApply = justParse $ stateP $ evaluate' prio opApply

arg ::
     (Show op, Show arg, Eq op, Eq arg)
  => Priority op
  -> OpApply op arg
  -> StateParser [Token op arg] arg
arg prio opApply =
  unconsSP_ >>= \case
    TLParen  -> evaluate' prio opApply <* ensureUnconsSP_ TRParen
    TConst v -> pure v
    a        -> failSP $ "arg: " ++ show a

evaluate' ::
     (Show op, Show arg, Eq op, Eq arg)
  => Priority op
  -> OpApply op arg
  -> StateParser [Token op arg] arg
evaluate' prio opApply = go []
  where
    go stk = do
      a <- arg prio opApply
      unconsSP >>= \case
        Nothing       -> pure $ unwindAll opApply stk a
        Just TRParen  -> putBackSP TRParen *> pure (unwindAll opApply stk a)
        Just (TOp op) -> go $ unwind prio opApply stk a op
        Just a        -> failSP $ "evaluate': " ++ show a

unwindAll :: OpApply op arg -> [(arg, op)] -> arg -> arg
unwindAll _ [] a                  = a
unwindAll opApply ((a, o):rest) b = unwindAll opApply rest $ opApply o a b

unwind ::
     Priority op -> OpApply op arg -> [(arg, op)] -> arg -> op -> [(arg, op)]
unwind _ _ [] a o = [(a, o)]
unwind prio opApply cur@((a, o1):rest) b o2
  | prio o2 > prio o1 = (b, o2) : cur
  | otherwise = unwind prio opApply rest (opApply o1 a b) o2
