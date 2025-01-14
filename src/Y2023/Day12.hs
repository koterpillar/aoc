module Y2023.Day12
  ( tasks
  ) where

import           Control.Monad.State

import           Control.Parallel.Strategies (parMap, rpar)

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text

import           AOC
import           Grid                        (middleDot)
import           Memo
import           Utils

data Pos
  = Y
  | Q
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Hashable Pos

posShow :: Maybe Pos -> Char
posShow (Just Y) = '#'
posShow (Just Q) = '?'
posShow Nothing  = middleDot

type Line = Map Int Pos

lineMin :: Line -> Maybe Int
lineMin = fmap fst . Map.lookupMin

lineMax :: Line -> Maybe Int
lineMax = fmap fst . Map.lookupMax

lineBounds :: Line -> Maybe (Int, Int)
lineBounds l = liftA2 (,) (lineMin l) (lineMax l)

lineInterval :: Int -> Int -> Line -> [Maybe Pos]
lineInterval k0 k1 ps = map (`Map.lookup` ps) [k0 .. k1]

lineList :: Line -> [Maybe Pos]
lineList ps =
  case lineBounds ps of
    Nothing       -> []
    Just (k0, k1) -> lineInterval k0 k1 ps

lineShow :: Line -> String
lineShow = map posShow . lineList

type Springs = [Int]

type InputLine = ([Maybe Pos], Springs)

parser :: Parser Text [InputLine]
parser =
  linesP
    &** wordsP
    &* ((charactersP &** choiceP [('.', Nothing), ('#', Just Y), ('?', Just Q)])
          &+ integersP ",")

mkLine' :: [Maybe Pos] -> Line
mkLine' p = Map.fromList [(i, c) | (i, Just c) <- zip [0 ..] p]

mkLine :: [Pos] -> Line
mkLine = mkLine' . map Just

posSplit :: Line -> Maybe (Line, Int, Line)
posSplit ps
  | null ks = Nothing
  | otherwise = Just (pl, k, pr)
  where
    ks = Map.keys $ Map.filter (== Q) ps
    k = ks !! (length ks `div` 2)
    (pl, p1) = Map.spanAntitone (< k) ps
    pr = Map.delete k p1

type PossibleCounts = Map Springs Int

countsAppend :: PossibleCounts -> PossibleCounts -> PossibleCounts
countsAppend a b =
  mapFromListSum $ do
    (ka, va) <- Map.toList a
    (kb, vb) <- Map.toList b
    pure (ka ++ kb, va * vb)

counts :: Line -> PossibleCounts
counts =
  unsafeMemo $ \ps ->
    case posSplit ps of
      Nothing -> Map.singleton (knownCounts ps) 1
      Just (pl, k, pr) -> (cl `countsAppend` cr) `mapSum` ca
        where cl = counts pl
              cr = counts pr
              ca = counts $ Map.insert k Y ps

lineLength :: Line -> Int
lineLength ps =
  fromMaybe 0 $ do
    (k0, k1) <- lineBounds ps
    pure $ k1 - k0 + 1

lineFirst :: Line -> Maybe (Line, Line)
lineFirst ps = do
  kmin <- lineMin ps
  let kend =
        fromJustE "lineFirst: kend" $ find (`Map.notMember` ps) [succ kmin ..]
  pure $ Map.spanAntitone (< kend) ps

lineFirstCount :: Line -> Maybe (Int, Line)
lineFirstCount ps = first lineLength <$> lineFirst ps

knownCounts :: Line -> Springs
knownCounts = unfoldr lineFirstCount

multiplyZ :: Int -> Int -> Int
multiplyZ 0 _ = 0
multiplyZ x y = x * y

productZ :: [Int] -> Int
productZ = foldl' multiplyZ 1

type PFT r = Line -> Springs -> r

fallback2 :: (a -> b -> Maybe c) -> (a -> b -> c) -> (a -> b -> c)
fallback2 heuristic slow a b = fromMaybe (slow a b) $ heuristic a b

infixr 1 `fallback2`
fallback2V ::
     (Eq c, Show a, Show b, Show c)
  => (a -> b -> Maybe c)
  -> (a -> b -> c)
  -> (a -> b -> c)
fallback2V heuristic slow a b =
  case heuristic a b of
    Nothing -> slow a b
    Just r ->
      let r' = slow a b
       in if r == r'
            then r
            else error
                   $ "heuristic failed on "
                       <> show a
                       <> ", "
                       <> show b
                       <> ": "
                       <> show r
                       <> " != "
                       <> show r'

infixr 1 `fallback2V`
possibilitiesCounts :: PFT Int
possibilitiesCounts ps cs =
  fromMaybe 0
    $ Map.lookup cs
    $ counts
    $ traceF (\l -> "brute force " <> lineShow l <> " " <> show cs) ps

cSize :: [Int] -> Int
cSize = sum . map succ

possibilitiesLargestAll :: PFT (Maybe Int)
possibilitiesLargestAll ps cs = do
  c <- maybeMaximum cs
  let (c1, _:c2) = span (/= c) cs
  (kMin, kMax) <- lineBounds ps
  let kMin' = kMin + cSize c1
  let kMax' = kMax - pred c - cSize c2
  let ks = filter (fits c ps) [kMin' .. kMax']
  let ks = filter (fits c ps) $ Map.keys ps
  when (length ks >= 60)
    $ traceM
    $ "largest branching "
        <> lineShow ps
        <> " into "
        <> show (length ks)
        <> " branches on "
        <> show c1
        <> "-"
        <> show c
        <> "-"
        <> show c2
  pure
    $ sum
        [ let [p1, p2] = cutParts c [k] ps
         in multiplyZ (possibilities0 p1 c1) (possibilities0 p2 c2)
        | k <- ks
        ]

cutParts :: Int -> [Int] -> Line -> [Line]
cutParts _ [] l = [l]
cutParts n (k:ks) l = l1 : cutParts n ks l2
  where
    l1 = Map.takeWhileAntitone (< k - 1) l
    l2 = Map.dropWhileAntitone (<= k + n) l

-- If there's only a single way to fit the largest numbers into blocks, put them
-- there and deal with the intervals in between separately.
-- This doesn't require fit points to be undamaged.
-- Examples:
-- ???..?##?..??? 1,4,1
possibilitiesLargestOnlyFit :: PFT (Maybe Int)
possibilitiesLargestOnlyFit ps cs = do
  c <- maybeMaximum cs
  let cparts = splitOn [c] cs
  let ks = filter (fits c ps) $ Map.keys ps
  guard $ and $ zipWithTail (\k1 k2 -> k1 + c < k2) ks
  guard $ length cparts == succ (length ks)
  let pparts = cutParts c ks ps
  pure $ productZ $ zipWith possibilities0 pparts cparts

isNeedle :: Int -> [Maybe Pos] -> Bool
isNeedle n xs =
  head xs /= Just Y
    && last xs /= Just Y
    && drop 1 (dropEnd 1 xs) == replicate n (Just Y)

-- If all the largest numbers fit each into a an undamaged ### block, put them
-- there and deal with the intervals in between separately.
-- Note that there might be other places where the largest numbers fit, but
-- that's irrelevant because of the undamaged blocks.
-- Examples:
-- ????###????###???? 1,3,1,3,1
possibilitiesLargestExactFit :: PFT (Maybe Int)
possibilitiesLargestExactFit ps cs = do
  c <- maybeMaximum cs
  let cparts = splitOn [c] cs
  let ks =
        filter (\k -> isNeedle c $ lineInterval (k - 1) (k + c) ps)
          $ Map.keys ps
  guard $ length cparts == succ (length ks)
  let pparts = cutParts c ks ps
  pure $ productZ $ zipWith possibilities0 pparts cparts

-- Does the interval fit onto the line in the specified position?
fits :: Int -> Line -> Int -> Bool
fits n ps k0 =
  Map.lookup (pred k0) ps /= Just Y
    && Map.lookup (succ k1) ps /= Just Y
    && notElem Nothing (lineInterval k0 k1 ps)
  where
    k1 = k0 + n - 1

-- Does the interval fit onto the line in the specified position, assuming it's
-- the first one?
fitsAsFirst :: Int -> Line -> Int -> Bool
fitsAsFirst n ps k0 =
  notElem (Just Y) (lineInterval kMin (pred k0) ps) && fits n ps k0
  where
    kMin = fromMaybe k0 $ lineMin ps

-- Does the interval fit onto the line in the specified position, assuming it's
-- the last one?
fitsAsLast :: Int -> Line -> Int -> Bool
fitsAsLast n ps k0 =
  notElem (Just Y) (lineInterval (succ k1) kMax ps) && fits n ps k0
  where
    kMax = fromMaybe k1 $ lineMax ps
    k1 = k0 + n - 1

-- If there's only one place where the first part fits, put it there and deal
-- with the rest recursively.
-- Examples:
-- #?. 1
-- ?#. 1
-- ?##. 3
possibilitiesAlignFirst :: PFT (Maybe Int)
possibilitiesAlignFirst ps cs = do
  (c1, crest) <- uncons cs
  (kMin, kMax) <- lineBounds ps
  [k] <- Just $ filter (fitsAsFirst c1 ps) [kMin .. kMax]
  let p1 = Map.dropWhileAntitone (<= k + c1) ps
  pure $ possibilities0 p1 crest

-- Same as above, but consider the last element.
possibilitiesAlignLast :: PFT (Maybe Int)
possibilitiesAlignLast ps cs = do
  (crest, c1) <- unsnoc cs
  (kMin, kMax) <- lineBounds ps
  let ks = filter (fitsAsLast c1 ps) [kMin .. kMax]
  [k] <- Just ks
  let p1 = Map.takeWhileAntitone (< k - 1) ps
  pure $ possibilities0 p1 crest

possibilitiesEmpty :: PFT (Maybe Int)
possibilitiesEmpty ps cs
  | null cs =
    Just
      $ if Y `elem` ps
          then 0
          else 1
  | Map.null ps = Just 0
  | otherwise = Nothing

possibilitiesSingle :: PFT (Maybe Int)
possibilitiesSingle ps [1]
  | Y `elem` ps = Nothing
  | otherwise = Just $ length ps
possibilitiesSingle _ _ = Nothing

possibilities0 :: PFT Int
possibilities0 =
  unsafeMemo2
    $ possibilitiesEmpty
        `fallback2` possibilitiesSingle
        `fallback2` possibilitiesAlignFirst
        `fallback2` possibilitiesAlignLast
        `fallback2` possibilitiesLargestExactFit
        `fallback2` possibilitiesLargestOnlyFit
        `fallback2` possibilitiesLargestAll
        `fallback2` possibilitiesCounts

possibilities :: InputLine -> Int
possibilities =
  uncurry possibilities0
    . first mkLine'
    . traceF (\(l, cs) -> "input " <> map posShow l <> " " <> show cs)

part1 :: [InputLine] -> Int
part1 = sum . parMap rpar (traceShowId . possibilities)

part2unfold :: InputLine -> InputLine
part2unfold = intercalate [Just Q] . replicate 5 *** join . replicate 5

part2 :: [InputLine] -> Int
part2 = part1 . map part2unfold

tasks =
  Tasks 2023 12 (CodeBlock 1) parser
    $ [ Assert "countsAppend" (Map.fromList [([], 1), ([1], 2), ([1, 1], 1)])
          $ let m = Map.fromList [([], 1), ([1], 1)]
             in countsAppend m m
      ]
        ++ [ let l' = mkLine l
            in Assert ("counts " <> Text.pack (lineShow l')) r $ counts l'
           | (l, r) <-
               [ ([Y], Map.fromList [([1], 1)])
               , ([Q, Y, Y, Q], Map.fromList [([2], 1), ([3], 2), ([4], 1)])
               , ( replicate 3 Q
                 , Map.fromList
                     [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)])
               , ( replicate 4 Q
                 , Map.fromList
                     [ ([], 1)
                     , ([1], 4)
                     , ([1, 1], 3)
                     , ([1, 2], 1)
                     , ([2], 3)
                     , ([2, 1], 1)
                     , ([3], 2)
                     , ([4], 1)
                     ])
               , ( replicate 5 Q
                 , Map.fromList
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
                     ])
               ]
           ]
        ++ [ Assert "possibilitiesAlignLast" (Just 1)
               $ possibilitiesAlignLast (mkLine [Y, Y, Y, Y]) [4]
           , Assert "possibilitiesAlignLast" Nothing
               $ possibilitiesAlignLast (mkLine [Q, Y, Q, Q]) [3]
           , Assert "possibilitiesSingle" (Just 20)
               $ possibilitiesSingle
                   (mkLine'
                      $ intercalate [Nothing]
                      $ replicate 4
                      $ replicate 5
                      $ Just Q)
                   [1]
           ]
        ++ [ AssertExample ("part 1 line " <> tshow i) r
             $ possibilities . flip (!!) i
           | (i, r) <- zip [0 ..] [1, 4, 1, 1, 4, 10]
           ]
        ++ [Task part1 21]
        ++ [ AssertExample ("part 2 line " <> tshow i) r
             $ possibilities . part2unfold . flip (!!) i
           | (i, r) <- zip [0 ..] [1, 16384, 1, 16, 2500, 506250]
           ]
        ++ [Task part2 525152]
