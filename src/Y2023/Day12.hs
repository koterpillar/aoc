module Y2023.Day12 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Pos1
  = Op1
  | Da1
  | Uk1
  deriving (Eq, Ord, Show, Bounded, Enum)

type Counts = [Int]

type Line = ([Pos1], Counts)

parser :: Parser Text [Line]
parser =
  linesP &** wordsP &* ((charactersP &** choiceEBP ".#?") &+ integersP ",")

data Pos
  = Da
  | Uk
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Memoizable Pos where
  memoize f t = memoize (f . toEnum) (fromEnum t)

pos1 :: Pos1 -> Pos
pos1 Op1 = error "Op1"
pos1 Da1 = Da
pos1 Uk1 = Uk

type Group = [Pos]

type Row = [Group]

groupP :: [Pos1] -> Row
groupP = map (map pos1) . filter (not . null) . splitOn [Op1]

groupL :: Line -> (Row, Counts)
groupL = first groupP

type MF2 a b c = (a -> b -> c) -> a -> b -> c

nullableGroup :: Group -> Bool
nullableGroup = all (== Uk)

matchGroup :: Group -> Int -> [Maybe Group]
matchGroup = memoFix2 matchGroup1

matchGroup1 :: MF2 Group Int [Maybe Group]
matchGroup1 _ [] n = []
matchGroup1 _ g@(Da:_) n
  | length g < n = []
  | length g <= n + 1 = [Nothing]
  | otherwise = [Just gr | d == Uk]
  where
    (gl, d:gr) = splitAt n g
matchGroup1 f (Uk:gs) n = f (Da : gs) n ++ f gs n

possibilities :: Line -> Int
possibilities = uncurry (memoFix2 poss1) . groupL

poss1 :: MF2 Row Counts Int
poss1 _ gs []
  | all nullableGroup gs = 1
  | otherwise = 0
poss1 _ [] _ = 0
poss1 f (g:gs) (c:cs) =
  sum [f (maybeToList g' ++ gs) cs | g' <- g1s] +
  sum [f gs (c : cs) | nullableGroup g]
  where
    g1s = matchGroup g c

part1 :: [Line] -> Int
part1 = sum . map possibilities

part2 :: [Line] -> Int
part2 = part1 . map (intercalate [Uk1] . replicate 5 *** join . replicate 5)

tasks =
  Tasks
    2023
    12
    (CodeBlock 1)
    parser
    [ AssertExample "second last line" 4 (possibilities . head . tail . reverse)
    , AssertExample "each line" [1, 4, 1, 1, 4, 10] (map possibilities)
    , Task part1 21
    , Task part2 525152
    ]
