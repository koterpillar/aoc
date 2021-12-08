import Data.List.Utils

import Data.Either

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String

import Utils

newtype Garbage =
  Garbage String
  deriving (Eq, Ord, Show)

type Contained = Either Group Garbage

newtype Group =
  Group [Contained]
  deriving (Eq, Ord, Show)

group :: Parser Group
group = do
  char '{'
  contents <- sepBy contained (char ',')
  char '}'
  pure $ Group contents

contained :: Parser Contained
contained = Left <$> group <|> Right <$> garbage

garbage :: Parser Garbage
garbage = do
  char '<'
  s <- many garbageItem
  char '>'
  pure $ Garbage $ concat s

garbageItem :: Parser String
garbageItem = (char '!' >> anyChar >> pure "") <|> (: []) <$> noneOf ">"

totalScore :: Group -> Int
totalScore = ts' 1
  where
    ts' ownScore (Group cs) =
      ownScore + sum (map (ts' (ownScore + 1)) (lefts cs))

totalGarbage :: Group -> Int
totalGarbage = tg . Left
  where
    tg (Left (Group cs)) = sum $ map tg cs
    tg (Right (Garbage g)) = length g

parsegroup :: String -> Group
parsegroup s =
  let (Right g) = parse group "" s
  in g
