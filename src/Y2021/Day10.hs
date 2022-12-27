module Y2021.Day10 where

import           AOC
import           Utils

data SyntaxError
  = Incomplete [Char]
  | Mismatch Char Char
  deriving (Eq, Show)

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing c   = error $ "not an opening bracket: " ++ show c

parseBR :: String -> Either SyntaxError ()
parseBR = go []
  where
    go st ('(':cs) = go ('(' : st) cs
    go st ('[':cs) = go ('[' : st) cs
    go st ('{':cs) = go ('{' : st) cs
    go st ('<':cs) = go ('<' : st) cs
    go ('(':st) (')':cs) = go st cs
    go ('[':st) (']':cs) = go st cs
    go ('{':st) ('}':cs) = go st cs
    go ('<':st) ('>':cs) = go st cs
    go (opening:_) (actual:_) = Left $ Mismatch (closing opening) actual
    go [] [] = Right ()
    go st [] = Left $ Incomplete $ map closing st
    go st cs = error $ "unexpected " <> cs <> " with stack " <> st

mismatchScore ')' = 3
mismatchScore ']' = 57
mismatchScore '}' = 1197
mismatchScore '>' = 25137
mismatchScore c   = error $ "not a closing bracket: " ++ show c

part1 :: [String] -> Int
part1 lns =
  sum
    [mismatchScore actual | Left (Mismatch expected actual) <- map parseBR lns]

incompleteScore :: [Char] -> Int
incompleteScore = go 0
  where
    go s []     = s
    go s (c:cs) = go (s * 5 + score c) cs
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score c   = error $ "not a closing bracket: " ++ show c

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

part2 lns =
  median
    [incompleteScore missing | Left (Incomplete missing) <- map parseBR lns]

tasks =
  Tasks
    2021
    10
    LastCodeBlock
    (linesP &** stringP)
    [ assert "incomplete" (Left $ Incomplete ")]}") (parseBR "{[(")
    , assert "median" 5 (median [5, 100, 0, 200, 1, 300, 3])
    , task part1 26397
    , task part2 288957
    ]
