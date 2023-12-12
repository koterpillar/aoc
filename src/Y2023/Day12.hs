module Y2023.Day12 where

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           GHC.IO.Handle.Types (Handle__)
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

type MF1 a b = (a -> b) -> a -> b

type MF2 a b c = (a -> b -> c) -> a -> b -> c

groupCounts :: Group -> Map Counts Int
groupCounts = memoFix gc1

gc1 :: MF1 Group (Map Counts Int)
gc1 _ [] = Map.singleton [] 1
gc1 f (Uk:ps) = f (Da : ps) `mapSum` f ps
gc1 f p@(Da:_)
  | null rest = Map.singleton [must] 1
  | otherwise =
    Map.mapKeys (must :) (f $ tail rest) `mapSum`
    f (take must p ++ (Da : tail rest))
  where
    must = length $ takeWhile (== Da) p
    rest = drop must p
    restR = f $ tail rest

addToFirst :: Num a => a -> [a] -> Maybe [a]
addToFirst a (b:c) = Just $ (a + b) : c
addToFirst _ _     = Nothing

possibilities :: Line -> Int
possibilities (p1s, cs) = poss (groupP p1s) cs
  where
    poss = poss1 poss

poss1 :: MF2 [Group] Counts Int
poss1 _ [] [] = 1
poss1 _ [] _ = 0
poss1 f (g:gs) cs =
  sum $ do
    (c, p) <- Map.toList $ groupCounts g
    guard $ c `isPrefixOf` cs
    let cs' = drop (length c) cs
    pure $ p * f gs cs'

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
    [ Assert "gc1 ?##?" (Map.fromList [([2], 1), ([3], 2), ([4], 1)]) $
      groupCounts [Uk, Da, Da, Uk]
    , Assert
        "gc1 ???"
        (Map.fromList [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)]) $
      groupCounts $ replicate 3 Uk
    , Assert
        "gc1 ????"
        (Map.fromList
           [ ([], 1)
           , ([1], 4)
           , ([1, 1], 3)
           , ([1, 2], 1)
           , ([2], 3)
           , ([2, 1], 1)
           , ([3], 2)
           , ([4], 1)
           ]) $
      groupCounts $ replicate 4 Uk
    , Assert
        "gc1 ?????"
        (Map.fromList
           [ ([], 1)
           , ([1], 5)
           , ([1, 1], 6)
           , ([1, 1, 1], 1)
           , ([1, 2], 3)
           , ([1, 3], 1)
           , ([2], 4)
           , ([2, 1], 3)
           , ([2, 2], 1)
           , ([3], 3)
           , ([3, 1], 1)
           , ([4], 2)
           , ([5], 1)
           ]) $
      groupCounts $ replicate 5 Uk
    , AssertExample "second last line" 4 $ possibilities . head . tail . reverse
    , AssertExample "each line" [1, 4, 1, 1, 4, 10] $ map possibilities
    , Task part1 21
    , Task part2 525152
    ]
