module Y2021.Day04 where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Miniparse
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

parsePlayP :: Parser Text Play
parsePlayP =
  linesP &* splitP [""] &* Parser splitHead &*
  (parseNumbersLine &= traverseP parseBoard) &*
  pureP (uncurry mkPlay)
  where
    splitHead (x:xs) = Right (x, xs)
    splitHead []     = Left "unexpected empty lines"
    parseNumbersLine = singleP &* integersP ","
    mkPlay numbers boards = Play boards numbers Nothing Set.empty
    parseBoardLine =
      tsplitP " " &* pureP (filter $ not . Text.null) &** integerP
    parseBoard = traverseP parseBoardLine &* Parser mkBoard
    mkBoard lns
      | length lns == bSize && all ((== bSize) . length) lns = Right $ Board lns
      | otherwise = error $ "Incorrect board size: " ++ show lns

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
