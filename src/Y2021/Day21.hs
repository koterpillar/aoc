module Y2021.Day21 where

import qualified Data.Map as Map

import           AOC
import           Quantum  (Collapse (..), Quantum)
import qualified Quantum
import           Utils

mod1 :: Int -> Int -> Int
mod1 a b =
  case mod a b of
    0 -> b
    x -> x

newtype QCount =
  QCount Int
  deriving (Eq, Ord, Show)

instance Collapse QCount where
  cinit = QCount 1
  collapse (QCount a) (QCount b) = QCount $ a + b
  cappend (QCount a) (QCount b) = QCount $ a * b

type Counts = Quantum QCount

cToSingle :: Show a => Quantum QCount a -> a
cToSingle c =
  case Quantum.toList c of
    [(x, QCount 1)] -> x
    vs              -> error $ "cToSingle: " ++ show vs

ctotal :: Counts a -> Int
ctotal cs = sum [c | (_, QCount c) <- Quantum.toList cs]

cexamples :: Show a => Counts a -> String
cexamples gs =
  "Total: " <>
  show (ctotal gs) <>
  " Examples: " <> intercalate ", " (map show $ take 2 $ Quantum.toList gs)

traceCounts :: Show a => Counts a -> Counts a
traceCounts = traceF cexamples

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
  Quantum.pure
    (clamp a + clamp (a + 1) + clamp (a + 2), Deterministic $ clamp $ a + 3)
  where
    clamp n = n `mod1` 100
roll Dirac =
  Quantum.fromListSingle
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
gameCurrentPlayer game = (gameTurn game + 1) `mod1` length (gamePositions game)

startGame :: Positions -> Game
startGame positions = Game {..}
  where
    gamePositions = mapFromList $ zip [1 ..] positions
    gameDice = Deterministic 1
    gameTurn = 0
    gameScore = mapFromList $ zip [1 ..] $ replicate (length positions) 0
    gameUntil = 1000

startGameDirac :: Positions -> Game
startGameDirac positions =
  (startGame positions) {gameDice = Dirac, gameUntil = 21}

gameStep :: Game -> Counts Game
gameStep g = Quantum.map (uncurry go) $ roll $ gameDice g
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
          mapLookup player (gamePositions g)
        position' = (position + movement) `mod1` 10
        gamePositions' = mapInsert player position' (gamePositions g)
        gameScore' = Map.adjust (+ position') player (gameScore g)

gameWin :: Game -> Maybe Player
gameWin g = fmap fst $ find ((>= gameUntil g) . snd) $ mapToList $ gameScore g

gameIsWin :: Game -> Bool
gameIsWin = isJust . gameWin

type Win = (Game, Player)

gamePlay :: Game -> Counts Win
gamePlay = go . Quantum.pure
  where
    go gs
      | Quantum.all gameIsWin gs =
        Quantum.map (\g -> (g, fromJust $ gameWin g)) gs
      | otherwise = go $ Quantum.flatMap gs stepWin
    stepWin g
      | gameIsWin g = Quantum.pure g
      | otherwise = gameStep g

part1 :: Positions -> Int
part1 game = loserScore * endTurn
  where
    (game', winner) = cToSingle $ gamePlay $ traceShowId $ startGame game
    [loser] = filter (/= winner) (gamePlayers game')
    Just loserScore = mapLookup loser (gameScore game')
    endTurn = gameTurn game' * 3

part2 :: Positions -> Int
part2 game = max s1 s2
  where
    [(_, QCount s1), (_, QCount s2)] =
      Quantum.toList $
      traceCounts $
      Quantum.map snd $
      traceCounts $ gamePlay $ traceShowId $ startGameDirac game

testDirac :: Counts Int
testDirac = Quantum.flatMap droll (\a -> Quantum.map (a +) droll)
  where
    droll = Quantum.map fst $ roll Dirac

tasks =
  Tasks
    2021
    21
    LastCodeBlock
    parse
    [ Task part1 739785
    , Assert "normal counts" (3 ^ 6) $ ctotal testDirac
    , Task part2 444356092776315
    ]

parse :: Parser Text Positions
parse = linesP &** tsplitP ": " &* pairPWith (\_ x -> x) (constP ()) integerP
