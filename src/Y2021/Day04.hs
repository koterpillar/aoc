module Y2021.Day04 where

import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

bSize :: Int
bSize = 5

type Board = Grid2 Int

bLines :: [[Position2]]
bLines = horizontal ++ vertical
  where
    horizontal =
      [[Position2 x y | x <- [0 .. bSize - 1]] | y <- [0 .. bSize - 1]]
    vertical = transpose horizontal

bWin :: Set Int -> Board -> Bool
bWin drawn board =
  or $ do
    line <- bLines
    let values = mapMaybe (`mapLookup` board) line
    pure $ all (`setMember` drawn) values

bScore :: Set Int -> Board -> Int
bScore drawn = sum . filter (not . (`setMember` drawn)) . toList

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
  lineGroupsP &* unconsP &* parseNumbersLine &= traverseP parseBoard &*
  pureP (uncurry mkPlay)
  where
    parseNumbersLine = singleP &* integersP ","
    mkPlay numbers boards = Play boards numbers Nothing mempty
    parseBoardLine = integersSpaceP
    parseBoard = traverseP parseBoardLine &* Parser mkBoard
    mkBoard lns
      | length lns == bSize && all ((== bSize) . length) lns =
        Right $ fromMatrixG lns
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
        { pPlayedNumbers = setInsert nextNumber pPlayedNumbers
        , pLastNumber = Just nextNumber
        , pNumbers = remainingNumbers
        , pBoards = filter (not . bWin pPlayedNumbers) pBoards
        }

pAllWins :: Play -> [Int]
pAllWins = concatMap pWinningScore . iterateMaybe pStep

part1 :: Play -> Int
part1 = head . pAllWins

part2 :: Play -> Int
part2 = last . pAllWins

tasks =
  Tasks
    2021
    4
    (CodeBlock 0)
    parsePlayP
    [ assert "Expected win" True $ bWin drawn board
    , assert "Board score" 3 $ bScore drawn board
    , assertExample "Parsed numbers count" 27 $ length . pNumbers
    , assertExample "Parsed board count" 3 $ length . pBoards
    , task part1 4512
    , task part2 1924
    ]
  where
    board = fromMatrixG [[1, 2], [3, 1]]
    drawn = setFromList [1, 2]
