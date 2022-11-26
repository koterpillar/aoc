module Y2020.Day18 where

import           Control.Monad.State

import           AOC
import           Utils

data Expr
  = ENum Int
  | EPlus Expr Expr
  | EMult Expr Expr
  deriving (Eq)

instance Show Expr where
  show (ENum n)      = show n
  show (EPlus e1 e2) = showE "+" e1 e2
  show (EMult e1 e2) = showE "*" e1 e2

showE :: String -> Expr -> Expr -> String
showE op e1 e2 = lp <> show e1 <> " " <> op <> " " <> show e2 <> rp
  where
    simple =
      case e1 of
        ENum _ -> True
        _      -> False
    lp =
      if simple
        then ""
        else "("
    rp =
      if simple
        then ""
        else ")"

data Token
  = TNum Int
  | TPlus
  | TMult
  | TLParen
  | TRParen
  deriving (Eq, Show)

flipToken :: Token -> Token
flipToken TLParen = TRParen
flipToken TRParen = TLParen
flipToken t       = t

tokenP :: Parser Text Token
tokenP =
  (TNum <$> integerP) &|
  choiceP [("+", TPlus), ("*", TMult), ("(", TLParen), (")", TRParen)]

parseExpr :: Parser Text Expr
parseExpr =
  pureP (treplace "(" "( " . treplace ")" " )") &* wordsP &** tokenP &*
  pureP (reverse . map flipToken) &* -- FIXME: to fix left-associativity
  stateP exprSP

type SP a = StateParser [Token] a

argSP :: SP Expr
argSP =
  unconsSP_ >>= \case
    TLParen -> do
      e <- exprSP
      ensureUnconsSP_ TRParen
      pure e
    TNum v -> pure $ ENum v
    a -> failSP $ "argSP: " ++ show a

exprSP :: SP Expr
exprSP = do
  a1 <- argSP
  unconsSP >>= \case
    Nothing -> pure a1
    Just TRParen -> do
      putBackSP TRParen
      pure a1
    Just TPlus -> do
      a2 <- exprSP
      pure $ EPlus a1 a2
    Just TMult -> do
      a2 <- exprSP
      pure $ EMult a1 a2
    Just a -> failSP $ "exprSP: " ++ show a

evaluate :: Expr -> Int
evaluate (ENum x)    = x
evaluate (EPlus x y) = evaluate x + evaluate y
evaluate (EMult x y) = evaluate x * evaluate y

part1 = sum . map evaluate

tasks = Tasks 2020 18 (InlineCode 4) (linesP &** parseExpr) [Task part1 12240]
