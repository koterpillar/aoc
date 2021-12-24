{-# LANGUAGE GADTs #-}

module Y2021.Day21 where

import qualified Data.Map as Map

import           AOC
import           Counts
import           Utils

mod1 :: Int -> Int -> Int
mod1 a b =
  case mod a b of
    0 -> b
    x -> x

type Positions = [Int]

type Player = Int

data Dice
  = Deterministic Int
  | Dirac
  deriving (Eq, Ord, Show)

diceSides :: [Int]
diceSides = [1, 2, 3]

roll :: Dice -> Counts (Int, Dice)
roll (Deterministic a) =
  cpure (clamp a + clamp (a + 1) + clamp (a + 2), Deterministic $ clamp $ a + 3)
  where
    clamp n = n `mod1` 100
roll Dirac =
  cFromList
    [(a + b + c, Dirac) | a <- diceSides, b <- diceSides, c <- diceSides]

data Game =
  Game
    { gamePositions :: !(Map Player Int)
    , gameDice      :: Dice
    , gameTurn      :: !Int
    , gameScore     :: !(Map Player Int)
    , gameUntil     :: !Int
    }
  deriving (Ord, Eq, Show)

gamePlayers :: Game -> [Player]
gamePlayers = Map.keys . gamePositions

gameCurrentPlayer :: Game -> Player
gameCurrentPlayer game =
  (gameTurn game + 1) `mod1` Map.size (gamePositions game)

startGame :: Positions -> Game
startGame positions = Game {..}
  where
    gamePositions = Map.fromList $ zip [1 ..] positions
    gameDice = Deterministic 1
    gameTurn = 0
    gameScore = Map.fromList $ zip [1 ..] $ replicate (length positions) 0
    gameUntil = 1000

startGameDirac :: Positions -> Game
startGameDirac positions =
  (startGame positions) {gameDice = Dirac, gameUntil = 21}

gameStep :: Game -> Counts Game
gameStep g = cmap (uncurry go) $ roll $ gameDice g
  where
    go movement dice' =
      g
        { gameTurn = succ $ gameTurn g
        , gamePositions = gamePositions'
        , gameScore = gameScore'
        , gameDice = dice'
        }
      where
        player = gameCurrentPlayer g
        position =
          fromJustE ("no position for player " <> show player) $
          Map.lookup player (gamePositions g)
        position' = (position + movement) `mod1` 10
        gamePositions' = Map.insert player position' (gamePositions g)
        gameScore' = Map.adjust (+ position') player (gameScore g)

gameWin :: Game -> Maybe Player
gameWin g = fmap fst $ find ((>= gameUntil g) . snd) $ Map.toList $ gameScore g

gameIsWin :: Game -> Bool
gameIsWin = isJust . gameWin

type Win = (Game, Player)

gamePlay :: Game -> Counts Win
gamePlay = go . cpure
  where
    go gs
      | call gameIsWin gs = cmapf gs $ \g -> (g, fromJust $ gameWin g)
      | otherwise = go $ cjoin $ cmap stepWin gs
    stepWin g
      | gameIsWin g = cpure g
      | otherwise = gameStep g

part1 :: Positions -> Int
part1 game = loserScore * endTurn
  where
    (game', winner) = cToSingle $ gamePlay $ traceShowId $ startGame game
    [loser] = filter (/= winner) (gamePlayers game')
    Just loserScore = Map.lookup loser (gameScore game')
    endTurn = gameTurn game' * 3

part2 :: Positions -> Int
part2 game = max s1 s2
  where
    [(_, s1), (_, s2)] =
      cToList $
      traceCounts $
      cmap snd $ traceCounts $ gamePlay $ traceShowId $ startGameDirac game

testDirac :: Counts Int
testDirac = cjoin $ cmap (\a -> cmap (a +) droll) droll
  where
    droll = cmap fst $ roll Dirac

tasks =
  Tasks
    2021
    21
    parse
    [ Task part1 739785
    , Assert "normal counts" (3 ^ 6) $ ctotal testDirac
    , Task part2 444356092776315
    ]

parse :: Parser Text Positions
parse = linesP &** (tsplitP ": " &* pairPWith (\_ x -> x) (constP ()) integerP)
