module Y2023.Day07 where

import           Data.Ord  (Down (..))

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Card
  = CA
  | CK
  | CQ
  | CJ
  | CT
  | C9
  | C8
  | C7
  | C6
  | C5
  | C4
  | C3
  | C2
  deriving (Show, Eq, Ord, Enum, Bounded)

cardP :: Parser Char Card
cardP = choiceEBP $ map (head . tail . show) (enumerate :: [Card])

newtype Hand =
  Hand
    { hCards :: [Card]
    }
  deriving (Eq)

instance Show Hand where
  show (Hand h) = "Hand " <> map (head . tail . show) h

parser :: Parser Text [(Hand, Integer)]
parser =
  linesP &** tsplitSpacesP &* ap2P (,) (Hand <$> (charactersP &** cardP)) readP

data HandType
  = FiveK
  | FourK
  | FullHouse
  | ThreeK
  | TwoPair
  | Pair
  | HighCard
  deriving (Show, Eq, Ord)

hType :: Hand -> HandType
hType (Hand h) =
  case sort $ Map.elems $ mapFromListCount h of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2]    -> Pair
    [1, 2, 2]       -> TwoPair
    [1, 1, 3]       -> ThreeK
    [2, 3]          -> FullHouse
    [1, 4]          -> FourK
    [5]             -> FiveK
    r               -> terror $ "Invalid hand" <> tshow r <> ": " <> tshow h

instance Ord Hand where
  compare = compare `on` (\h -> (hType h, hCards h))

part1 = sum . zipWith winning [1 ..] . sortOn (Down . fst)
  where
    winning rank (h, bid) =
      traceShowF
        (\r ->
           tshow h <>
           " type " <>
           tshow (hType h) <>
           " rank " <> tshow rank <> " bid " <> tshow bid <> " = " <> tshow r)
        (rank * bid * 1)

tasks = Tasks 2023 7 (CodeBlock 0) parser [Task part1 6440]
