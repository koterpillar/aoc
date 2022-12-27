module Y2020.Day16 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Search
import           Utils

newtype Rule =
  Rule
    { unRule :: [(Int, Int)]
    }
  deriving (Show)

ruleP :: Parser Text Rule
ruleP = Rule <$> tsplitP " or " &** tsplitP "-" &* integerP &+ integerP

newtype Ticket =
  Ticket
    { unTicket :: [Int]
    }
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
  lineGroupsP &*
  ap3P
    Notes
    (mapFromList <$> traverseP (tsplitP ": " &* idP &+ ruleP))
    (pureP tail &* singleP &* ticketP)
    (pureP tail &** ticketP)

valid :: Rule -> Int -> Bool
valid (Rule bounds) x = any (\(lo, hi) -> inRange lo hi x) bounds

invalid1 :: Notes -> Ticket -> [Int]
invalid1 Notes {nFields = rs} (Ticket fs) =
  filter (\f -> not $ any (flip valid f) rs) fs

part1 n = sum $ concatMap (invalid1 n) (nTickets n)

nValidTickets :: Notes -> [Ticket]
nValidTickets n = filter (null . invalid1 n) $ nTickets n

ticketSets :: Notes -> Map Int (Set Int)
ticketSets =
  Map.fromList .
  zipN 0 . map Set.fromList . transpose . map unTicket . nValidTickets

ruleSets :: Notes -> Map Text (Set Int)
ruleSets =
  Map.map (mconcat . map (Set.fromList . uncurry enumFromTo) . unRule) . nFields

rulePossibilities :: Map Int (Set Int) -> Set Int -> Set Int
rulePossibilities tsets range =
  Set.fromList $ mapFilterValues (`Set.isSubsetOf` range) tsets

rulesPossibilities :: Notes -> Map Text (Set Int)
rulesPossibilities n = Map.map (rulePossibilities (ticketSets n)) (ruleSets n)

part2 n =
  product $
  Map.filterWithKey (\t _ -> Text.isPrefixOf "departure" t) $
  (\assignment -> decode assignment (nOwnTicket n)) $
  decideMapping $ rulesPossibilities n

decode :: Map Text Int -> Ticket -> Map Text Int
decode assignment (Ticket fs) = Map.mapWithKey (\t i -> fs !! i) assignment

tasks = Tasks 2020 16 (CodeBlock 1) notesP [task part1 71, task part2 1]
