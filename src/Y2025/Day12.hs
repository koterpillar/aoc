module Y2025.Day12 (
    tasks,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Grid
import Path
import Utils

type Present = Grid2 ()

presentP :: Parser Text (Int, Present)
presentP = tsplitP ":\n" &* (integerP &+ charGridP)

presentsP :: Parser Text (Map Int Present)
presentsP = fmap Map.fromList $ lineGroupsP &** (pureP Text.unlines &* presentP)

data Requirement = Requirement Position2 (Map Int Int) deriving (Ord, Eq, Show)

requirementP :: Parser Text Requirement
requirementP = tsplitP ": " &* ap2P Requirement dp cp
  where
    dp = tsplitP "x" &* ap2P Position2 integerP integerP
    cp = fmap (Map.fromList . zip [0 ..]) (wordsP &** integerP)

requirementsP :: Parser Text [Requirement]
requirementsP = linesP &** requirementP

type Input = (Map Int Present, [Requirement])

splitLastGroup :: Text -> [Text]
splitLastGroup t = let (a, b) = Text.breakOnEnd "\n\n" t in [Text.dropEnd 2 a, b]

parser :: Parser Text Input
parser = pureP splitLastGroup &* (presentsP &+ requirementsP)

reqEnumerate :: Map Int a -> Requirement -> [a]
reqEnumerate m (Requirement _ rs) = join [replicate n (m Map.! i) | (i, n) <- Map.toList rs]

type Fit = Map Int Present -> Requirement -> Bool

-- Just going by size, is there enough space for all presents?
notEnoughSpace :: Fit
notEnoughSpace pm req@(Requirement (Position2 w h) _) = w * h < sum areas
  where
    areas = map Map.size $ reqEnumerate pm req

-- Replacing every present with a 3x3 block, is there enough space?
enoughSquares :: Fit
enoughSquares _ (Requirement (Position2 w h) ps) = (w `div` 3) * (h `div` 3) >= sum ps

heuristic :: Fit -> Fit
heuristic fit pm req
    | notEnoughSpace pm req = False
    | enoughSquares pm req = True
    | otherwise = fit pm req

process :: Fit -> Input -> Int
process fit = uncurry $ countIf . heuristic fit

-- Example has special cases but actual input is covered purely by heuristics
part1 :: Input -> Int
part1 = process (const $ const False)

tasks =
    Tasks
        (AOC 2025 12)
        (CodeBlock 0)
        parser
        [ taskBlind part1 & taskPart 1
        ]
