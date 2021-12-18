module Y2021.Day18 where

import           Control.Monad.State
import qualified Data.Text           as Text

import           AOC
import           Utils

data Snail a
  = PNumber Int
  | PPair (Snail a) (Snail a)
  deriving (Ord, Eq)

instance Show a => Show (Snail a) where
  show (PNumber n)   = show n
  show (PPair n1 n2) = "[" <> show n1 <> "," <> show n2 <> "]"

type SnailInt = Snail Int

magnitude :: SnailInt -> Int
magnitude (PNumber n)   = n
magnitude (PPair n1 n2) = 3 * magnitude n1 + 2 * magnitude n2

snadd :: SnailInt -> SnailInt -> SnailInt
snadd n1 n2 = snreduce $ PPair n1 n2

snreduce :: SnailInt -> SnailInt
snreduce = undefined

part1 :: [SnailInt] -> Int
part1 = magnitude . foldl1 snadd

tasks =
  Tasks
    2021
    18
    (linesP &** pureP parse)
    [ Assert
        "reduce"
        (parse "[[[[0,9],2],3],4]")
        (snreduce $ parse "[[[[[9,8],1],2],3],4]")
    , Task part1 4140
    ]

parseS :: State String SnailInt
parseS = do
  c <- getchar
  case c of
    '[' -> do
      n1 <- parseS
      ensure ','
      n2 <- parseS
      ensure ']'
      return $ PPair n1 n2
    d
      | isDigit d -> do
        n <- parseNumber
        return $ PNumber $ read $ d : n
      | otherwise -> sterror $ "expected number or [, got " <> show c

parse :: Text -> SnailInt
parse input =
  let (number, []) = runState parseS (Text.unpack input)
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
