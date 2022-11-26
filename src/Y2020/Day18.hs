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

arg :: SP Int -> SP Int
arg ev =
  unconsSP_ >>= \case
    TLParen -> ev <* ensureUnconsSP_ TRParen
    TNum v  -> pure v
    a       -> failSP $ "arg: " ++ show a

opApply :: Op -> Int -> Int -> Int
opApply OPlus = (+)
opApply OMult = (*)

evaluate1 :: SP Int
evaluate1 = go 0 OPlus
  where
    go a op = do
      b <- arg evaluate1
      let c = opApply op a b
      unconsSP >>= \case
        Nothing        -> pure c
        Just (TOp op2) -> go c op2
        Just TRParen   -> putBackSP TRParen *> pure c
        Just a         -> failSP $ "evaluate1: " ++ show a

part1 = sum . map (runSP evaluate1)

tasks = Tasks 2020 18 (InlineCode 4) (linesP &** parseExpr) [Task part1 12240]
