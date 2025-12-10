module Y2025.Day10 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Path
import Utils
import Z3A

data Machine = Machine {mLights :: Set Int, mButtons :: [Set Int], mJoltage :: Map Int Int} deriving (Eq, Ord, Show, Generic)

instance Hashable Machine

parser :: Parser Text [Machine]
parser = linesP &** fmap (\(t, (b, j)) -> Machine t b j) (tsplitP "] " &* (lightsP &+ (tsplitP " {" &* (buttonsP &+ joltageP))))

lightsP :: Parser Text (Set Int)
lightsP = pureP (Text.drop 1) &* charactersP &* pureP mkSet
  where
    mkSet = Set.fromList . map fst . filter (\(_, c) -> c == '#') . zip [0 ..]

buttonsP :: Parser Text [Set Int]
buttonsP = wordsP &** fmap Set.fromList (pureP (Text.drop 1 . Text.dropEnd 1) &* tsplitP "," &** integerP)

joltageP :: Parser Text (Map Int Int)
joltageP = fmap (Map.fromList . zip [0 ..]) (pureP (Text.dropEnd 1) &* tsplitP "," &** integerP)

solve1 :: Machine -> Int
solve1 m = length $ fromJustE "solve1" $ aStarDepthGoal moves estimate init
  where
    init = mLights m
    moves l = [toggle l b | b <- mButtons m]
    estimate lights
        | Set.null lights = 0
        | otherwise = 1

toggle :: Set Int -> Set Int -> Set Int
toggle a b = Set.difference a b `Set.union` Set.difference b a

part1 :: [Machine] -> Int
part1 = sum . map solve1

solve2 :: Machine -> Integer
solve2 = sum . runZ3 . solve2z

buttonsFor :: Int -> Machine -> [Int]
buttonsFor k = filterTuple (Set.member k) . zip [0 ..] . mButtons

solve2z :: Machine -> Z3 [Integer]
solve2z m@(Machine _ buttons target) = do
    zButtons <- replicateM (length buttons) (mkFreshIntVar "b")
    _0 <- mkInteger 0
    for_ zButtons $ \b -> optimizeAssert =<< mkGe b _0
    for_ (Map.toList target) $ \(k, v) -> do
        zResult <- mkInteger (fromIntegral v)
        let relevant = [zButtons !! i | i <- buttonsFor k m]
        optimizeAssert =<< mkEq zResult =<< mkAdd relevant
    optimizeMinimize =<< mkAdd zButtons
    m <- optimizeRun
    for zButtons $ fmap fromJust . evalInt m

part2 :: [Machine] -> Integer
part2 = sum . map solve2

tasks =
    Tasks
        2025
        10
        (CodeBlock 0)
        parser
        [ task part1 7 & taskPart 1
        , task part2 33 & taskPart 2
        ]
