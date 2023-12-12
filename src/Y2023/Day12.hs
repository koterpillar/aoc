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

gc1 :: Group -> Map Counts Int
gc1 = memoFix gc1m

gc1m :: MF1 Group (Map Counts Int)
gc1m _ [] = Map.singleton [] 1
gc1m f (Uk:ps) = f (Da : ps) `mapSum` f ps
gc1m f p@(Da:_)
  | null rest = Map.singleton [must] 1
  | otherwise =
    Map.mapKeys (must :) (f $ tail rest) `mapSum`
    f (take must p ++ (Da : tail rest))
  where
    must = length $ takeWhile (== Da) p
    rest = drop must p
    restR = f $ tail rest

gcMerge :: Map Counts Int -> Map Counts Int -> Map Counts Int
gcMerge a b =
  Map.fromList $ do
    (ka, va) <- Map.toList a
    (kb, vb) <- Map.toList b
    pure (ka ++ kb, va * vb)

gc3 :: Row -> Map Counts Int
gc3 gs = foldr1 gcMerge $ map gc1 gs

possibilities :: Line -> Int
possibilities (p1s, cs) = fromMaybe 0 $ Map.lookup cs m
  where
    gs = groupP p1s
    m = gc3 gs

part1 :: [Line] -> Int
part1 = sum . map possibilities

part2unfold :: Line -> Line
part2unfold = intercalate [Uk1] . replicate 5 *** join . replicate 5

part2 :: [Line] -> Int
part2 = part1 . map part2unfold

tasks =
  Tasks
    2023
    12
    (CodeBlock 1)
    parser
    [ Assert "gc1 ?##?" (Map.fromList [([2], 1), ([3], 2), ([4], 1)]) $
      gc1 [Uk, Da, Da, Uk]
    , Assert
        "gc1 ???"
        (Map.fromList [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)]) $
      gc1 $ replicate 3 Uk
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
      gc1 $ replicate 4 Uk
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
      gc1 $ replicate 5 Uk
    , AssertExample "second last line" 4 $ possibilities . head . tail . reverse
    , AssertExample "each line" [1, 4, 1, 1, 4, 10] $ map possibilities
    , Task part1 21
    , AssertExample "first line 2" 1 $ possibilities . part2unfold . head
    , AssertExample "second line 2" 16384 $ possibilities . part2unfold . head . tail
    , AssertExample "each line 2" [1, 16384, 1, 16, 2500, 506250] $
      map (possibilities . part2unfold)
    , Task part2 525152
    ]
