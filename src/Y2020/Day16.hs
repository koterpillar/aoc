module Y2020.Day16 where

import           AOC
import           Utils

newtype Rule =
  Rule [(Int, Int)]
  deriving (Show)

ruleP :: Parser Text Rule
ruleP = Rule <$> tsplitP " or " &** (tsplitP "-" &* (integerP &+ integerP))

newtype Ticket =
  Ticket [Int]
  deriving (Show)

ticketP :: Parser Text Ticket
ticketP = Ticket <$> integersP ","

data Notes =
  Notes
    { nFields    :: Map Text Rule
    , nOwnTicket :: Ticket
    , nTickets   :: [Ticket]
    }
  deriving (Show)

notesP :: Parser Text Notes
notesP =
  (\(r, (t, ts)) -> Notes r t ts) <$>
  lineGroupsP &* headTailP &*
  ((mapFromList <$> traverseP (tsplitP ": " &* (idP &+ ruleP))) &=
   (pureP tail &* singleP &* ticketP &+ pureP tail &** ticketP))

valid :: Rule -> Int -> Bool
valid (Rule bounds) x = any (\(lo, hi) -> inRange lo hi x) bounds

invalid1 :: [Rule] -> Ticket -> [Int]
invalid1 rs (Ticket fs) = filter (\f -> not $ any (flip valid f) rs) fs

part1 (Notes fs _ ts) = sum $ concatMap (invalid1 (toList fs)) ts

tasks = Tasks 2020 16 (CodeBlock 1) notesP [Task part1 71]
