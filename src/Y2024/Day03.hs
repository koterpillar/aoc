module Y2024.Day03
  ( tasks
  ) where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

data Instr
  = Mul Int Int
  | Enable Bool
  deriving (Eq, Ord, Show)

result :: [Instr] -> Int
result = go True 0
  where
    go enabled acc []            = acc
    go enabled acc (Enable b:xs) = go b acc xs
    go True acc (Mul x y:xs)     = go True (acc + x * y) xs
    go False acc (Mul _ _:xs)    = go False acc xs

mulExpr :: StateParser String Instr
mulExpr = do
  ensureStartSP "mul("
  x <- naturalSP
  ensureStartSP ","
  y <- naturalSP
  ensureStartSP ")"
  pure $ Mul x y

doExpr :: StateParser String Instr
doExpr = ensureStartSP "do()" $> Enable True

dontExpr :: StateParser String Instr
dontExpr = ensureStartSP "don't()" $> Enable False

part2Expr :: StateParser String Instr
part2Expr = mulExpr `altSP` doExpr `altSP` dontExpr

parseStart :: StateParser a b -> a -> Maybe b
parseStart p s =
  case runStateT p s of
    Left _       -> Nothing
    Right (x, _) -> Just x

parseIn :: StateParser [a] b -> [a] -> [b]
parseIn p = join . map (toList . parseStart p) . tails

part1 :: String -> Int
part1 = result . parseIn mulExpr

part2 :: String -> Int
part2 = result . parseIn part2Expr

tasks =
  Tasks
    2024
    3
    (CodeBlock 0)
    stringP
    [ task part1 161 & taskPart 1
    , task part2 48 & taskPart 2 & taskScraper (CodeBlock 1)
    ]
