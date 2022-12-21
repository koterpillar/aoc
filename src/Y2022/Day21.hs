module Y2022.Day21 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Expr
  = Const Int
  | Op Char Text Text
  deriving (Ord, Eq, Show)

exprP :: Parser Text Expr
exprP =
  (Const <$> integerP) &|
  (mk <$> (wordsP &* unconsP &* (idP &= (charP &+ idP))))
  where
    mk (a, (c, b)) = Op c a b

parser :: Parser Text (Map Text Expr)
parser = Map.fromList <$> linesP &** tsplitP ": " &* idP &+ exprP

evaluate :: Text -> Map Text Expr -> Int
evaluate k m = ev1 $ mapLookupE "evaluate" k m
  where
    ev1 (Const a)  = a
    ev1 (Op c a b) = op c (evaluate a m) (evaluate b m)

op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = div

part1 = evaluate "root"

tasks = Tasks 2022 21 (CodeBlock 0) parser [Task part1 152]
