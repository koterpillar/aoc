module Y2023.Day12 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Grid                (middleDot)
import           Utils

data Pos
  = Y
  | Q
  deriving (Eq, Ord, Show, Bounded, Enum)

type Line = Map Int Pos

lineMin :: Line -> Maybe Int
lineMin = fmap fst . Map.lookupMin

lineMax :: Line -> Maybe Int
lineMax = fmap fst . Map.lookupMax

lineShow :: Line -> String
lineShow l
  | Map.null l = ""
  | otherwise =
    map
      (item . (`Map.lookup` l))
      [fromJustE "lineShow" $ lineMin l .. fromJustE "lineShow" $ lineMax l]
  where
    item (Just Y) = '#'
    item (Just Q) = '?'
    item Nothing  = middleDot

type Springs = [Int]

type InputLine = ([Maybe Pos], Springs)

parser :: Parser Text [InputLine]
parser =
  linesP &** wordsP &*
  ((charactersP &** choiceP [('.', Nothing), ('#', Just Y), ('?', Just Q)]) &+
   integersP ",")

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

countsM :: MemoState Line PossibleCounts
countsM ps =
  memoState ps $
  case posSplit ps of
    Nothing -> pure $ Map.singleton (knownCounts ps) 1
    Just (pl, k, pr) -> do
      cl <- countsM pl
      cr <- countsM pr
      ca <- countsM $ Map.insert k Y ps
      pure $ (cl `countsAppend` cr) `mapSum` ca

lineLength :: Line -> Int
lineLength ps = fromMaybe 0 $ (-) <$> lineMax ps <*> lineMin ps

lineFirst :: Line -> Maybe (Line, Line)
lineFirst ps = do
  kmin <- lineMin ps
  let kend =
        fromJustE "lineFirst: kend" $ find (`Map.notMember` ps) [succ kmin ..]
  pure $ Map.spanAntitone (< kend) ps

lineFirstCount :: Line -> Maybe (Int, Line)
lineFirstCount ps = first lineLength <$> lineFirst ps

knownCounts :: Line -> Springs
knownCounts = unfoldr go
  where
    go :: Line -> Maybe (Int, Line)
    go ps = do
      kmin <- lineMin ps
      let kend =
            fromJustE "knownCounts: kend" $
            find (`Map.notMember` ps) [succ kmin ..]
      pure (kend - kmin, Map.dropWhileAntitone (< kend) ps)

type PFT r = Line -> Springs -> r

fallback :: PFT (Maybe r) -> PFT r -> PFT r
fallback heuristic slow p c = fromMaybe (slow p c) $ heuristic p c

infixr 1 `fallback`

possibilitiesCounts :: PFT Int
possibilitiesCounts ps cs =
  fromMaybe 0 $
  Map.lookup cs $
  counts $ traceF (\l -> "last resort " <> lineShow l <> " " <> show cs) ps

possibilitiesAlignFirst :: PFT (Maybe Int)
possibilitiesAlignFirst ps cs = do
  (c1:crest) <- Just cs
  k0 <- lineMin ps
  let k1 = k0 + c1
  let p1 = map (`Map.lookup` ps) [k0 .. k1]
  case p1 of
    (Just Y:_) -> cutAt crest k1
    (Just Q:Just Y:_) ->
      if last p1 == Just Y
        then cutAt crest $ succ k1
        else Nothing
    _ -> Nothing
  where
    cutAt cs k = do
      guard $ Map.lookup k ps /= Just Y
      pure $ possibilities0 (Map.dropWhileAntitone (<= k) ps) cs

possibilities0 :: PFT Int
possibilities0 = possibilitiesAlignFirst `fallback` possibilitiesCounts

possibilities :: InputLine -> Int
possibilities = uncurry possibilities0 . first mkLine' . traceShowF ("input", )

part1 :: [InputLine] -> Int
part1 = sum . map (traceShowId . possibilities)

part2unfold :: InputLine -> InputLine
part2unfold = intercalate [Just Q] . replicate 5 *** join . replicate 5

part2 :: [InputLine] -> Int
part2 = part1 . map part2unfold

tasks =
  Tasks 2023 12 (CodeBlock 1) parser $
  [ Assert "countsAppend" (Map.fromList [([], 1), ([1], 2), ([1, 1], 1)]) $
    let m = Map.fromList [([], 1), ([1], 1)]
     in countsAppend m m
  , Assert "counts ?##?" (Map.fromList [([2], 1), ([3], 2), ([4], 1)]) $
    counts $ mkLine [Q, Y, Y, Q]
  , Assert
      "counts ???"
      (Map.fromList [([], 1), ([1], 3), ([1, 1], 1), ([2], 2), ([3], 1)]) $
    counts $ mkLine $ replicate 3 Q
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
    counts $ mkLine $ replicate 4 Q
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
    counts $ mkLine $ replicate 5 Q
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
