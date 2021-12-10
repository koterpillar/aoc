import           Data.Text   (Text)
import qualified Data.Text   as Text

import           Debug.Trace

import           AOC
import           Utils

data SyntaxError
  = Incomplete [Char]
  | Mismatch Char Char
  deriving (Show)

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'

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
    go st [] = Left $ Incomplete st
    go st cs = error $ "unexpected " <> cs <> " with stack " <> st

mismatchScore ')' = 3
mismatchScore ']' = 57
mismatchScore '}' = 1197
mismatchScore '>' = 25137

part1 lns =
  sum $
  map
    mismatchScore
    [actual | Left (Mismatch expected actual) <- map parseBR lns]

main :: IO ()
main = do
  processEI 10 (map Text.unpack . Text.lines) part1 26397
