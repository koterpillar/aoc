module Y2024.Day03 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

parser :: Parser Text String
parser = pureP Text.unpack

ensureStr :: String -> StateParser String ()
ensureStr = mapM_ ensureUnconsSP_

mulExpr :: StateParser String (Int, Int)
mulExpr = do
  ensureStr "mul("
  x <- naturalSP
  ensureStr ","
  y <- naturalSP
  ensureStr ")"
  pure (x, y)

parseStart :: StateParser a b -> a -> Maybe b
parseStart p s =
  case runStateT p s of
    Left _       -> Nothing
    Right (x, _) -> Just x

parseIn :: StateParser [a] b -> [a] -> [b]
parseIn p = join . map (toList . parseStart p) . tails

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . parseIn mulExpr

data Instr
  = Mul (Int, Int)
  | Enable Bool
  deriving (Eq, Ord, Show)

result :: [Instr] -> Int
result = go True 0
  where
    go enabled acc []            = acc
    go enabled acc (Enable b:xs) = go b acc xs
    go True acc (Mul (x, y):xs)  = go True (acc + x * y) xs
    go False acc (Mul _:xs)      = go False acc xs

mulExpr' :: StateParser String Instr
mulExpr' = Mul <$> mulExpr

doExpr :: StateParser String Instr
doExpr = ensureStr "do()" $> Enable True

dontExpr :: StateParser String Instr
dontExpr = ensureStr "don't()" $> Enable False

part2Expr :: StateParser String Instr
part2Expr = mulExpr' `altSP` doExpr `altSP` dontExpr

part2 = result . parseIn part2Expr

tasks =
  Tasks
    2024
    3
    (CodeBlock 0)
    parser
    [ task part1 161 & taskPart 1
    , task part2 48 & taskPart 2 & taskScraper (CodeBlock 1)
    ]
