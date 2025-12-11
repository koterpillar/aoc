module Y2020.Day18
  ( tasks
  ) where

import           AOC
import           Evaluator
import           Utils

data Op
  = OPlus
  | OMult
  deriving (Eq, Show, Enum, Bounded)

type T = Token Op Int

parseExpr :: Parser Text [T]
parseExpr = expressionP ["+", "*"] integerP

opApply :: Op -> Int -> Int -> Int
opApply OPlus = (+)
opApply OMult = (*)

priority1 :: Priority Op
priority1 = const 1

priority2 :: Priority Op
priority2 OPlus = 2
priority2 OMult = 1

part1 = sum . map (evaluate priority1 opApply)

part2 = sum . map (evaluate priority2 opApply)

normalPriority :: Priority Op
normalPriority OPlus = 1
normalPriority OMult = 2

normalEvaluate = evaluate normalPriority opApply

tasks =
  Tasks
    (AOC 2020 18)
    (InlineCode 4)
    (linesP &** parseExpr)
    [ Assert
        "normalEvaluate"
        (1 * 2 + 3 * 4)
        (normalEvaluate $ justParse parseExpr "1 * 2 + 3 * 4")
    , Task part1 12240
    , Task part2 669060
    ]
