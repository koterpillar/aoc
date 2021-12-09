{-# LANGUAGE RecordWildCards #-}

import           Control.Monad    (join)

import           Data.List
import           Data.List.Split  (splitOn)

import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Text.Parsec
import           Text.Parsec.Text

import           Debug.Trace

import           AOC
import           Utils

bSize :: Int
bSize = 5

data Board =
  Board
    { bValues :: [[Int]]
    }
  deriving (Eq, Show)

nullBoard :: Board
nullBoard = Board []

bLines :: Board -> [[Int]]
bLines (Board b) = b ++ transpose b

bWin :: Set Int -> Board -> Bool
bWin drawn board =
  or $ do
    line <- bLines board
    pure $ all (`Set.member` drawn) line

bScore :: Set Int -> Board -> Int
bScore drawn = sum . filter (`Set.notMember` drawn) . join . bValues

data Play =
  Play
    { pBoards        :: [Board]
    , pNumbers       :: [Int]
    , pLastNumber    :: Maybe Int
    , pPlayedNumbers :: Set Int
    }
  deriving (Eq, Show)

playP :: Parser Play
playP =
  mkPlay <$> sepBy integerP (char ',') <* nl <* nl <*> sepBy numberLineP nl <*
  eof
  where
    numberLineP = skipMany spc *> sepBy integerP (many1 spc)
    mkPlay numbers lines = Play (sepBoards lines) numbers Nothing Set.empty
    sepBoards =
      map assertCorrectSize . filter (/= nullBoard) . map Board . splitOn [[]]
    assertCorrectSize (Board b)
      | length b == bSize && all ((== bSize) . length) b = Board b
      | otherwise = error $ "Incorrect board size: " ++ show b
    spc = char ' '
    nl = char '\n'

pWinningScore :: Play -> Maybe Int
pWinningScore Play {..} = do
  board <- find (bWin pPlayedNumbers) pBoards
  lastNumber <- pLastNumber
  pure $ bScore pPlayedNumbers board * lastNumber

pStep :: Play -> Play
pStep play@Play {..} =
  case pNumbers of
    [] -> error "No more numbers to draw"
    nextNumber:remainingNumbers ->
      play
        { pPlayedNumbers = Set.insert nextNumber pPlayedNumbers
        , pLastNumber = Just nextNumber
        , pNumbers = remainingNumbers
        }

part1 :: Play -> Int
part1 play =
  case pWinningScore play of
    Just score -> score
    Nothing    -> part1 $ pStep play

main = do
  let board = Board [[1, 2], [3, 1]]
  let drawn = Set.fromList [1, 2]
  assertEqual "Expected win" True $ bWin drawn board
  assertEqual "Board score" 3 $ bScore drawn board
  example <- getExample 4
  let examplePlay = parse playP "" example
  assertEqual "Parsed numbers count" (Right 27) $
    length . pNumbers <$> examplePlay
  assertEqual "Parsed board count" (Right 3) $ length . pBoards <$> examplePlay
  processEI 4 (justParse playP) part1 4512
