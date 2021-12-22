{-# LANGUAGE GADTs #-}

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

newtype Counts a =
  Counts
    { getCounts :: Map a Int
    }
  deriving (Eq, Ord, Show)

cpure :: Ord a => a -> Counts a
cpure x = Counts $ Map.singleton x 1

cFromListCount :: Ord a => [(a, Int)] -> Counts a
cFromListCount = Counts . mapFromListSum

cFromList :: Ord a => [a] -> Counts a
cFromList = cFromListCount . flip zip (repeat 1)

cToList :: Counts a -> [(a, Int)]
cToList = Map.toList . getCounts

cToSingle :: Show a => Counts a -> a
cToSingle c =
  case cToList c of
    [(x, 1)] -> x
    vs       -> error $ "cToSingle: " ++ show vs

ctotal :: Counts a -> Int
ctotal = sum . Map.elems . getCounts

cmap :: Ord b => (a -> b) -> Counts a -> Counts b
cmap f = Counts . Map.mapKeys f . getCounts

cmapf :: Ord b => Counts a -> (a -> b) -> Counts b
cmapf = flip cmap

cjoin :: Ord a => Counts (Counts a) -> Counts a
cjoin = Counts . mapFromListSum . concatMap toListMul . cToList
  where
    toListMul (c, n) = [(v, m * n) | (v, m) <- cToList c]

call :: (a -> Bool) -> Counts a -> Bool
call f = all f . Map.keys . getCounts

crights :: Ord b => Counts (Either a b) -> Maybe (Counts b)
crights = fmap cFromListCount . traverse go . cToList
  where
    go (Right b, c) = Just (b, c)
    go _            = Nothing

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

type Win = (Game, Player)

gamePlay :: Game -> Counts Win
gamePlay = go . cmap Left . cpure
  where
    go gs =
      case crights gs of
        Just wins -> wins
        Nothing ->
          go $
          traceShowF (cmap (bimap gameTurn (const "win"))) $
          cjoin $ cmap (either stepWin (cpure . Right)) gs
    stepWin g =
      case gameWin g of
        Just p  -> cpure $ Right (g, p)
        Nothing -> cmap Left $ gameStep g

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
      cToList $ cmap snd $ gamePlay $ traceShowId $ startGameDirac game

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
