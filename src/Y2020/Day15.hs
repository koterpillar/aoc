module Y2020.Day15 where

import           AOC
import           Utils

data State =
  State
    { sTurns    :: !Int
    , sRecent   :: !(Map Int Int)
    , sLastSaid :: !(Maybe Int)
    }
  deriving (Show)

say :: Int -> State -> State
say n s =
  s
    { sTurns = sTurns s + 1
    , sRecent =
        case sLastSaid s of
          Nothing -> sRecent s
          Just p  -> mapInsert p (sTurns s) (sRecent s)
    , sLastSaid = Just n
    }

nextNumber :: State -> Int
nextNumber State {..} =
  case sLastSaid of
    Nothing -> error "nextNumber: no last said"
    Just n ->
      case mapLookup n sRecent of
        Nothing -> 0
        Just p  -> sTurns - p

emptyState :: State
emptyState = State {sTurns = 0, sRecent = mempty, sLastSaid = Nothing}

turn :: State -> State
turn s = say (nextNumber s) s

part1 n input = fromJust $ sLastSaid $ iterateN (n - length input) turn start
  where
    start = foldl' (flip say) emptyState input

tasks =
  Tasks
    2020
    15
    (Inline "0,3,6")
    (tsplitP "," &** integerP)
    [Task (part1 2020) 436, Task (part1 30000000) 175594]
