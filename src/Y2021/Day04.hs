module Y2021.Day04 where

import           Data.Text        (Text)
import qualified Data.Text        as Text

import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

bSize :: Int
bSize = 5

newtype Board =
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

parsePlayP :: Text -> Play
parsePlayP = go . Text.lines
  where
    go (nl:_:boards) =
      mkPlay (parseNumbersLine nl) (sepBoards $ map parseBoardLine boards)
    go _ = error "unexpected lines"
    mkPlay numbers boards = Play boards numbers Nothing Set.empty
    parseNumbersLine = map tread . Text.splitOn ","
    parseBoardLine = map tread . filter (not . Text.null) . Text.splitOn " "
    sepBoards =
      map assertCorrectSize . filter (/= nullBoard) . map Board . splitOn [[]]
    assertCorrectSize (Board b)
      | length b == bSize && all ((== bSize) . length) b = Board b
      | otherwise = error $ "Incorrect board size: " ++ show b

pWinningScore :: Play -> [Int]
pWinningScore Play {..} = do
  board <- filter (bWin pPlayedNumbers) pBoards
  lastNumber <- maybeToList pLastNumber
  pure $ bScore pPlayedNumbers board * lastNumber

pStep :: Play -> Maybe Play
pStep play@Play {..} =
  case pNumbers of
    [] -> Nothing
    nextNumber:remainingNumbers ->
      Just $
      play
        { pPlayedNumbers = Set.insert nextNumber pPlayedNumbers
        , pLastNumber = Just nextNumber
        , pNumbers = remainingNumbers
        , pBoards = filter (not . bWin pPlayedNumbers) pBoards
        }

pAllWins :: Play -> [Int]
pAllWins play = pWinningScore play ++ rest (pStep play)
  where
    rest (Just p) = pAllWins p
    rest Nothing  = []

part1 :: Play -> Int
part1 = head . pAllWins

part2 :: Play -> Int
part2 = last . pAllWins

tasks =
  Tasks
    2021
    4
    parsePlayP
    [ Assert "Expected win" True $ bWin drawn board
    , Assert "Board score" 3 $ bScore drawn board
    , AssertExample "Parsed numbers count" 27 $ length . pNumbers
    , AssertExample "Parsed board count" 3 $ length . pBoards
    , Task part1 4512
    , Task part2 1924
    ]
  where
    board = Board [[1, 2], [3, 1]]
    drawn = Set.fromList [1, 2]
