module Y2020.Day22 where

import           AOC
import           Utils

type Card = Int

type Hand = [Card]

handP :: Parser [Text] Hand
handP = pureP tail &** integerP

score :: Hand -> Int
score = sum . zipWith (*) [1 ..] . reverse

type Game = (Hand, Hand)

gameP :: Parser Text Game
gameP = lineGroupsP &* (handP &+ handP)

move :: Game -> Either Hand Game
move ([], h) = Left h
move (h, []) = Left h
move (c1:h1, c2:h2)
  | c1 > c2 = Right (h1 ++ [c1, c2], h2)
  | otherwise = Right (h1, h2 ++ [c2, c1])

play :: Game -> Hand
play game =
  case move game of
    Left result -> result
    Right game' -> play game'

tasks = Tasks 2020 22 (CodeBlock 0) gameP [Task (score . play) 306]
