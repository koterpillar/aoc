module Y2023.Day12 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

data Pos
  = N
  | Y
  | Q
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Memoizable Pos where
  memoize f t = memoize (f . toEnum) (fromEnum t)

type Springs = [Int]

type Line = ([Pos], Springs)

parser :: Parser Text [Line]
parser =
  linesP &** wordsP &* ((charactersP &** choiceEBP ".#?") &+ integersP ",")

posSplit :: [Pos] -> Maybe ([Pos], [Pos])
posSplit ps
  | null ixs = Nothing
  | otherwise = Just (pl, pr)
  where
    ixs = elemIndices Q ps
    ixm = ixs !! (length ixs `div` 2)
    (pl, _:pr) = splitAt ixm ps

type Counts = Map Springs Int

countsAppend :: Counts -> Counts -> Counts
countsAppend a b =
  mapFromListSum $ do
    (ka, va) <- Map.toList a
    (kb, vb) <- Map.toList b
    pure (ka ++ kb, va * vb)

optimizePos :: [Pos] -> [Pos]
optimizePos []     = []
optimizePos (N:ps) = N : optimizePos (dropWhile (== N) ps)
optimizePos (p:ps) = p : optimizePos ps

counts :: [Pos] -> Counts
counts = runMemo . countsM

type MemoState i o = i -> State (Map i o) o

memoState :: Ord i => i -> State (Map i o) o -> State (Map i o) o
memoState k a = do
  existing <- gets (Map.lookup k)
  case existing of
    Just result -> pure result
    Nothing -> do
      calculated <- a
      modify $ Map.insert k calculated
      pure calculated

runMemo :: State (Map i o) r -> r
runMemo = flip evalState Map.empty

countsM :: MemoState [Pos] Counts
countsM ps =
  memoState ps $
  case posSplit ps of
    Nothing -> pure $ Map.singleton (knownCounts ps) 1
    Just (pl, pr) -> do
      cl <- countsM pl
      cr <- countsM pr
      let pa = pl ++ (Y : pr)
      ca <- countsM pa
      pure $ (cl `countsAppend` cr) `mapSum` ca

knownCounts :: [Pos] -> Springs
knownCounts ps = unfoldr kc1 ps
  where
    kc1 ps
      | null p1 = Nothing
      | otherwise = Just (l2, p3)
      where
        p1 = dropWhile (== N) ps
        l2 = length $ takeWhile (== Y) p1
        p3 = drop l2 p1

type PFT r = [Pos] -> Springs -> r

fallback :: PFT r -> PFT (Maybe r) -> PFT r
fallback slow heuristic p c = fromMaybe (slow p c) $ heuristic p c

possibilities3 :: PFT Int
possibilities3 ps cs =
  fromMaybe 0 $ Map.lookup cs $ counts $ traceShowF ("last resort", , cs) ps

possibilities2 :: PFT Int
possibilities2 =
  fallback possibilities3 $ \ps cs -> do
    guard $ notNull cs
    let cmax = maximum cs
    let cparts = splitOn [cmax] cs
    let pparts = splitOn (replicate cmax Y) (surround ps)
    guard $ length pparts == length cparts
    pseared <- traverse sear pparts
    -- traceShowM ("p2 -> ", cmax, zip pseared cparts)
    pure $ product $ zipWith possibilities0 pseared cparts
  where
    surround x = N : (x ++ [N])

sear :: [Pos] -> Maybe [Pos]
sear = searStart >=> (pure . reverse) >=> searStart >=> (pure . reverse)

searStart :: [Pos] -> Maybe [Pos]
searStart []    = Nothing
searStart (Y:_) = Nothing
searStart (_:t) = Just t

anchorFor :: Int -> [Pos] -> Maybe (Int, [Pos])
anchorFor c ps = do
  let pfirst = takeWhile (/= N) ps
  guard $ elem Y pfirst
  guard $ length pfirst <= c
  let prest = drop (succ $ length pfirst) ps
  Just (length pfirst - c + 1, prest)

possibilities1 :: PFT Int
possibilities1 =
  fallback possibilities2 $ \ps cs -> do
    (c:cs1) <- pure cs
    (r1, prest) <- anchorFor c ps
    Just $ r1 * possibilities0 prest cs1

possibilities0 :: PFT Int
possibilities0 ps = possibilities1 (optimizePos ps)

possibilities :: Line -> Int
possibilities = uncurry possibilities0 . traceShowF ("input", )

part1 :: [Line] -> Int
part1 = sum . map (traceShowId . possibilities)

part2unfold :: Line -> Line
part2unfold = intercalate [Q] . replicate 5 *** join . replicate 5

part2 :: [Line] -> Int
part2 = part1 . map part2unfold

tasks =
  Tasks 2023 12 (CodeBlock 1) parser $
  [ Assert "countsAppend" (Map.fromList [([], 1), ([1], 2), ([1, 1], 1)]) $
    let m = Map.fromList [([], 1), ([1], 1)]
     in countsAppend m m
  , Assert "counts ?##?" (Map.fromList [([2], 1), ([3], 2), ([4], 1)]) $
    counts [Q, Y, Y, Q]
  , Assert
      "counts ???"
      (Map.fromList [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)]) $
    counts $ replicate 3 Q
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
    counts $ replicate 4 Q
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
    counts $ replicate 5 Q
  ] ++
  [ AssertExample ("part 1 line " <> tshow i) r $ possibilities . flip (!!) i
  | (i, r) <- zip [0 ..] [1, 4, 1, 1, 4, 10]
  ] ++
  [Task part1 21] ++
  [ AssertExample ("part 2 line " <> tshow i) r $
  possibilities . part2unfold . flip (!!) i
  | (i, r) <- zip [0 ..] [1, 16384, 1, 16, 2500, 506250]
  ] ++
  [Task part2 525152]
