module Y2021.Day18 where

import           Control.Monad.State
import qualified Data.Text           as Text

import           AOC
import           Utils

data Snailnumber
  = PNumber Int
  | PPair Snailnumber Snailnumber
  deriving (Ord, Eq, Show)

magnitude :: Snailnumber -> Int
magnitude (PNumber n)   = n
magnitude (PPair n1 n2) = 3 * magnitude n1 + 2 * magnitude n2

snadd :: Snailnumber -> Snailnumber -> Snailnumber
snadd n1 n2 = snreduce $ PPair n1 n2

snreduce :: Snailnumber -> Snailnumber
snreduce = _

part1 :: [Snailnumber] -> Int
part1 = magnitude . foldl1 snadd

tasks = Tasks 2021 18 (linesP &** parse') [Task part1 4140]

parse :: State String Snailnumber
parse = do
  c <- getchar
  case c of
    '[' -> do
      n1 <- parse
      ensure ','
      n2 <- parse
      ensure ']'
      return $ PPair n1 n2
    d
      | isDigit d -> do
        n <- parseNumber
        return $ PNumber $ read $ d : n
      | otherwise -> sterror $ "expected number or [, got " <> show c

parse' :: Parser Text Snailnumber
parse' =
  pureP $ \input ->
    let (number, []) = runState parse (Text.unpack input)
     in number

getchar :: State String Char
getchar = state $ fromJust . uncons

sterror :: String -> State String a
sterror message = do
  rem <- get
  error $ message <> " remainder: " <> show rem

ensure :: Char -> State String ()
ensure c =
  getchar >>= \c' ->
    if c == c'
      then pure ()
      else sterror $ "expected " <> show c <> " but got " <> show c'

parseNumber :: State String [Char]
parseNumber =
  gets head >>= \c ->
    if isDigit c
      then (c :) <$> parseNumber
      else pure []
