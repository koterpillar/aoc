{-# LANGUAGE GADTs #-}

module Y2021.Day21 where

import           Control.Monad.Identity

import qualified Data.Map               as Map

import           AOC
import           Utils

newtype Counts m =
  Counts
    { getCounts :: [(m, Int)]
    }
  deriving (Ord, Eq, Show)

instance Functor Counts where
  fmap f (Counts as) = Counts [(f a, b) | (a, b) <- as]

instance Applicative Counts where
  pure a = Counts [(a, 1)]
  liftA2 f (Counts as) (Counts bs) =
    Counts [(f a b, ca * cb) | (a, ca) <- as, (b, cb) <- bs]

instance Monad Counts where
  Counts as >>= f =
    Counts [(b, ca * cb) | (a, ca) <- as, (b, cb) <- getCounts (f a)]

class Shrink m where
  shrink :: Ord a => m a -> m a

instance Shrink Identity where
  shrink = id

instance Shrink Counts where
  shrink = Counts . Map.toList . mapFromListSum . getCounts

mod1 :: Int -> Int -> Int
mod1 a b =
  case mod a b of
    0 -> b
    x -> x

type Positions = [Int]

type Player = Int

data Dice m where
  Deterministic :: Int -> Dice Identity
  Dirac :: Dice Counts

instance Eq (Dice m) where
  Deterministic a == Deterministic b = a == b
  Dirac == Dirac = True

instance Ord (Dice m) where
  Deterministic a `compare` Deterministic b = a `compare` b
  Dirac `compare` Dirac = EQ

roll :: Dice m -> m (Int, Dice m)
roll (Deterministic a) = pure (a, Deterministic $ a + 1 `mod1` 100)
roll Dirac             = Counts [((n, Dirac), 1) | n <- [1, 2, 3]]

data Game m =
  Game
    { gamePositions :: !(Map Player Int)
    , gameDice      :: Dice m
    , gameTurn      :: !Int
    , gameScore     :: !(Map Player Int)
    , gameUntil     :: !Int
    }
  deriving (Ord, Eq)

instance Show (Game m) where
  show Game {..} =
    "Game: pos=" ++
    show gamePositions ++
    " turn=" ++ show gameTurn ++ " score=" ++ show gameScore

gameThrowDice :: Monad m => Game m -> m (Int, Game m)
gameThrowDice game = do
  (v1, d1) <- roll $ gameDice game
  (v2, d2) <- roll d1
  (v3, d3) <- roll d2
  pure (v1 + v2 + v3, game {gameDice = d3})

gamePlayers :: Game m -> [Player]
gamePlayers = Map.keys . gamePositions

gameCurrentPlayer :: Game m -> Player
gameCurrentPlayer game =
  (gameTurn game + 1) `mod1` Map.size (gamePositions game)

startGame :: Positions -> Game Identity
startGame positions = Game {..}
  where
    gamePositions = Map.fromList $ zip [1 ..] positions
    gameDice = Deterministic 1
    gameTurn = 0
    gameScore = Map.fromList $ zip [1 ..] $ replicate (length positions) 0
    gameUntil = 1000

startGameDirac :: Positions -> Game Counts
startGameDirac positions =
  (startGame positions) {gameDice = Dirac, gameUntil = 21}

gameStep :: (Monad m, Shrink m) => Game m -> m (Game m)
gameStep g0 = uncurry go <$> shrink (gameThrowDice g0)
  where
    go movement g =
      g
        { gameTurn = succ $ gameTurn g
        , gamePositions = gamePositions'
        , gameScore = gameScore'
        }
      where
        player = gameCurrentPlayer g
        position =
          fromJustE ("no position for player " <> show player) $
          Map.lookup player (gamePositions g)
        position' = (position + movement) `mod1` 10
        gamePositions' = Map.insert player position' (gamePositions g)
        gameScore' = Map.adjust (+ position') player (gameScore g)

gameWin :: Game m -> Maybe Player
gameWin g = fmap fst $ find ((>= gameUntil g) . snd) $ Map.toList $ gameScore g

gamePlay :: (Monad m, Shrink m) => Game m -> m (Game m, Player)
gamePlay g =
  case gameWin g of
    Just player -> pure (g, player)
    Nothing     -> gameStep g >>= gamePlay

part1 :: Positions -> Int
part1 game = loserScore * endTurn
  where
    (game', winner) = runIdentity $ gamePlay $ traceShowId $ startGame game
    [loser] = filter (/= winner) (gamePlayers game')
    Just loserScore = Map.lookup loser (gameScore game')
    endTurn = gameTurn game' * 3

part2 :: Positions -> Int
part2 = undefined

tasks = Tasks 2021 21 parse [Task part1 739785, Task part2 444356092776315]

parse :: Parser Text Positions
parse = linesP &** (tsplitP ": " &* pairPWith (\_ x -> x) (constP ()) integerP)
