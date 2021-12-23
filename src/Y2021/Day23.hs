module Y2021.Day23 where

import qualified Data.Map as Map

import AOC
import Utils
import Grid

data Amphi = A | B | C | D deriving (Ord, Eq, Show)

aEnergy :: Amphi -> Int
aEnergy A = 1
aEnergy B = 10
aEnergy C = 100
aEnergy D = 1000

part1 = undefined

tasks = Tasks 2021 23 parse [Task part1 12521]

parse :: Parser Text (Grid2 Amphi)
parse = Map.mapMaybe id . fromMatrixG <$> linesP &** (charactersP &** parseChar)

parseChar :: Parser Char (Maybe Amphi)
parseChar = choiceP $ [('A', Just A), ('B', Just B), ('C', Just C), ('D', Just D)] ++ [(c, Nothing) | c <- ".# "]
