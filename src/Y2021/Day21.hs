module Y2021.Day21 where

import qualified Data.Map as Map

import           AOC
import           Utils

mod1 :: Int -> Int -> Int
mod1 a b =
  case mod a b of
    0 -> b
    x -> x

type Positions = [Int]

type Player = Int

data Game =
  Game
    { gamePositions :: !(Map Player Int)
    , gameDice      :: [Int]
    , gameTurn      :: !Int
    , gameScore     :: !(Map Player Int)
    }

instance Show Game where
  show Game {..} =
    "Game: pos=" ++
    show gamePositions ++
    " dice=" ++
    show (head gameDice) ++
    "... turn=" ++ show gameTurn ++ " score=" ++ show gameScore

gamePlayers :: Game -> [Player]
gamePlayers = Map.keys . gamePositions

gameCurrentPlayer :: Game -> Player
gameCurrentPlayer game =
  (gameTurn game + 1) `mod1` Map.size (gamePositions game)

startGame :: Positions -> Game
startGame positions = Game {..}
  where
    gamePositions = Map.fromList $ zip [1 ..] positions
    gameDice = cycle [1 .. 100]
    gameTurn = 0
    gameScore = Map.fromList $ zip [1 ..] $ replicate (length positions) 0

gameStep :: Game -> Game
gameStep g =
  traceShowId $
  g
    { gameTurn = succ $ gameTurn g
    , gamePositions = gamePositions'
    , gameDice = gameDice'
    , gameScore = gameScore'
    }
  where
    d1:d2:d3:gameDice' = gameDice g
    movement = d1 + d2 + d3
    player = gameCurrentPlayer g
    position =
      fromJustE ("no position for player " <> show player) $
      Map.lookup player (gamePositions g)
    position' = (position + movement) `mod1` 10
    gamePositions' = Map.insert player position' (gamePositions g)
    gameScore' = Map.adjust (+ position') player (gameScore g)

gameWin :: Game -> Maybe Player
gameWin = fmap fst . find ((>= 1000) . snd) . Map.toList . gameScore

gamePlay :: Game -> (Game, Player)
gamePlay g =
  case gameWin g of
    Just player -> (g, player)
    Nothing     -> gamePlay $ gameStep g

part1 :: Positions -> Int
part1 game = loserScore * endTurn
  where
    (game', winner) = gamePlay $ traceShowId $ startGame $ traceShowId game
    [loser] = filter (/= winner) (gamePlayers game')
    Just loserScore = Map.lookup loser (gameScore game')
    endTurn = gameTurn game' * 3

tasks = Tasks 2021 21 parse [Task part1 739785]

parse :: Parser Text Positions
parse = linesP &** (tsplitP ": " &* pairPWith (\_ x -> x) (constP ()) integerP)
