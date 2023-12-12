module Y2023.Day12 where

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           GHC.IO.Handle.Types (Handle__)
import           Utils

data Pos
  = Op
  | Da
  | Uk
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Memoizable Pos where
  memoize f t = memoize (f . toEnum) (fromEnum t)

type MF1 a b = (a -> b) -> a -> b

type MF2 a b c = (a -> b -> c) -> a -> b -> c

type Counts = [Int]

type Line = ([Pos], Counts)

parser :: Parser Text [Line]
parser =
  linesP &** wordsP &* ((charactersP &** choiceEBP ".#?") &+ integersP ",")

posSplit :: [Pos] -> Maybe ([Pos], [Pos])
posSplit ps
  | null ixs = Nothing
  | otherwise = Just (pl, pr)
  where
    ixs = elemIndices Uk ps
    ixm = ixs !! (length ixs `div` 2)
    (pl, _:pr) = splitAt ixm ps

countsAppend :: Map Counts Int -> Map Counts Int -> Map Counts Int
countsAppend a b =
  mapFromListSum $ do
    (ka, va) <- Map.toList a
    (kb, vb) <- Map.toList b
    pure (ka ++ kb, va * vb)

counts :: [Pos] -> Map Counts Int
counts =
  memoFix $ \f ps ->
    case posSplit ps of
      Nothing -> Map.singleton (knownCounts ps) 1
      Just (pl, pr) -> (cl `countsAppend` cr) `mapSum` ca
        where cl = f pl
              cr = f pr
              pa = pl ++ (Da : pr)
              ca = f pa

knownCounts :: [Pos] -> Counts
knownCounts ps = unfoldr kc1 ps
  where
    kc1 ps
      | null p1 = Nothing
      | otherwise = Just (l2, p3)
      where
        p1 = dropWhile (== Op) ps
        l2 = length $ takeWhile (== Da) p1
        p3 = drop l2 p1

possibilities :: Line -> Int
possibilities (ps, cs) = fromMaybe 0 $ Map.lookup cs $ counts ps

part1 :: [Line] -> Int
part1 = sum . map (traceShowId . possibilities)

part2unfold :: Line -> Line
part2unfold = intercalate [Uk] . replicate 5 *** join . replicate 5

part2 :: [Line] -> Int
part2 = part1 . map part2unfold

tasks =
  Tasks
    2023
    12
    (CodeBlock 1)
    parser
    [ Assert "countsAppend" (Map.fromList [([], 1), ([1], 2), ([1, 1], 1)]) $
      let m = Map.fromList [([], 1), ([1], 1)]
       in countsAppend m m
    , Assert "counts ?##?" (Map.fromList [([2], 1), ([3], 2), ([4], 1)]) $
      counts [Uk, Da, Da, Uk]
    , Assert
        "counts ???"
        (Map.fromList [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)]) $
      counts $ replicate 3 Uk
    , Assert
        "counts ????"
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
      counts $ replicate 4 Uk
    , Assert
        "counts ?????"
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
      counts $ replicate 5 Uk
    , AssertExample "second last line" 4 $ possibilities . head . tail . reverse
    , AssertExample "each line" [1, 4, 1, 1, 4, 10] $ map possibilities
    , Task part1 21
    , AssertExample "first line 2" 1 $ possibilities . part2unfold . head
    , AssertExample "second line 2" 16384 $
      possibilities . part2unfold . head . tail
    , AssertExample "each line 2" [1, 16384, 1, 16, 2500, 506250] $
      map (possibilities . part2unfold)
    , Task part2 525152
    ]
