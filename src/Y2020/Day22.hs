module Y2020.Day22
  ( tasks
  ) where

import           Control.Monad.State

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
gameP = lineGroupsP &* handP &+ handP

move :: Game -> Either Hand Game
move ([], h) = Left h
move (h, []) = Left h
move (c1:h1, c2:h2)
  | c1 > c2 = Right (h1 ++ [c1, c2], h2)
  | otherwise = Right (h1, h2 ++ [c2, c1])

play :: Game -> Hand
play = iterateEither move

type Game2 = (Set Game, Game)

type Result2 = Either Hand Hand

move2 :: Game2 -> Either Result2 Game2
move2 (seen, g)
  | setMember g seen = Left $ Left $ fst g
  | otherwise =
    case g of
      ([], h2) -> Left $ Right h2
      (h1, []) -> Left $ Left h1
      (c1:h1, c2:h2)
        | length h1 >= c1 && length h2 >= c2 ->
          case play2 (take c1 h1, take c2 h2) of
            Left _  -> toP1
            Right _ -> toP2
        | c1 > c2 -> toP1
        | otherwise -> toP2
        where toP1 = Right (setInsert g seen, (h1 ++ [c1, c2], h2))
              toP2 = Right (setInsert g seen, (h1, h2 ++ [c2, c1]))

play2 :: Game -> Result2
play2 game = iterateEither move2 (mempty, game)

winHand :: Result2 -> Hand
winHand = either id id

tasks =
  Tasks
    (AOC 2020 22)
    (CodeBlock 0)
    gameP
    [Task (score . play) 306, Task (score . winHand . play2) 291]
