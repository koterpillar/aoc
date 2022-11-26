module Y2020.Day16 where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Path
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

invalid1 :: Notes -> Ticket -> [Int]
invalid1 Notes {nFields = rs} (Ticket fs) =
  filter (\f -> not $ any (flip valid f) rs) fs

part1 n = sum $ concatMap (invalid1 n) (nTickets n)

nValidTickets :: Notes -> [Ticket]
nValidTickets n = filter (null . invalid1 n) (nTickets n)

data Search =
  Search
    { sFound      :: Map Text Int
    , sRemNames   :: [Text]
    , sRemIndices :: Set Int
    }
  deriving (Ord, Eq, Show)

instance Hashable Search where
  hashWithSalt x (Search a b c) = hashWithSalt x (a, b, c)

valid2 :: Map Text Rule -> Map Text Int -> Bool
valid2 rules = all (uncurry v) . mapToList
  where
    v field value =
      case Map.lookup field rules of
        Nothing   -> True
        Just rule -> valid rule value

match :: Notes -> Map Text Int
match n =
  sFound $
  traceShowId $
  lastE "empty route found" $
  fromJustE "no route found" $
  aStar
    moves
    (const $ const 1)
    (length . sRemIndices)
    (null . sRemIndices)
    start
  where
    vt = nValidTickets n
    allValid assignment = all (valid2 (nFields n) . decode assignment) vt
    moves = hashSetFromList . filter (allValid . sFound) . moves'
    moves' Search {..} =
      let (name:names) = sRemNames
       in [ Search
            (mapInsert name i sFound)
            names
            (setDifference sRemIndices $ set1 i)
          | i <- toList sRemIndices
          ]
    start =
      let fs = nFields n
       in Search mempty (Map.keys fs) (setFromList [0 .. length fs - 1])

part2 n =
  product $
  Map.filterWithKey (\t _ -> Text.isPrefixOf "departure" t) $
  (\assignment -> decode assignment (nOwnTicket n)) $ match n

decode :: Map Text Int -> Ticket -> Map Text Int
decode assignment (Ticket fs) = Map.mapWithKey (\t i -> fs !! i) assignment

tasks = Tasks 2020 16 (CodeBlock 1) notesP [Task part1 71, Task part2 1]
