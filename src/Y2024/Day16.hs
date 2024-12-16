module Y2024.Day16
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

data Item
  = Wall
  | Start
  | End
  deriving (Ord, Eq, Show, Enum, Bounded)

instance GridItem Item where
  showInGrid Wall  = '#'
  showInGrid Start = 'S'
  showInGrid End   = 'E'

type Grid = Grid2 Item

parser :: Parser Text Grid
parser = charGridP

data Deer =
  Deer Position2 Direction4
  deriving (Ord, Eq, Show)

instance Hashable Deer where
  hashWithSalt s (Deer p d) = hashWithSalt s (p, d)

type Path = [Deer]

type Seen = Set Position2

mkSeen :: Path -> Seen
mkSeen = Set.fromList . map (\(Deer p _) -> p)

bestPath :: Seen -> Grid -> Maybe Path
bestPath ps g = (start :) <$> aStarGoal (moves g) (score2 ps) remaining start
  where
    start = Deer deerStart E
    deerStart = fromJustE "deerStart" $ findTuple (== Start) $ Map.toList g
    deerEnd = fromJustE "deerEnd" $ findTuple (== End) $ Map.toList g
    remaining (Deer p _) = manhattanDistance p deerEnd

bestPaths :: Grid -> [Path]
bestPaths g =
  case bestPath mempty g of
    Nothing -> []
    Just p1 -> p1 : unfoldr go (mkSeen p1)
      where go ps =
              case bestPath ps g of
                Nothing -> Nothing
                Just p
                  | score p == p1score ->
                    let s = mkSeen p
                     in if s `Set.isSubsetOf` ps
                          then Nothing
                          else Just (p, s <> ps)
                  | otherwise -> Nothing
            p1score = score p1

moves :: Grid -> Deer -> Path
moves g (Deer p d) =
  turns ++ [Deer (walk d p) d | Map.lookup (walk d p) g /= Just Wall]
  where
    turns = [Deer p $ turnLeft d, Deer p $ turnRight d]

score1 :: Deer -> Deer -> Int
score1 (Deer p1 d1) (Deer p2 d2)
  | d1 == d2 = 1
  | otherwise = 1000

score2 :: Seen -> Deer -> Deer -> Int
score2 seen d1 d2 = score1 d1 d2 * 1000000 + seenScore d1 + seenScore d2
  where
    seenScore (Deer p _) =
      if Set.member p seen
        then 1
        else 0

score :: Path -> Int
score = sum . zipWithTail score1

part1 :: Grid -> Int
part1 = score . fromJustE "no best path" . bestPath mempty

part2 :: Grid -> Int
part2 g =
  (length . Set.unions . map mkSeen . ttraceF (displayPath g . join) . bestPaths)
    g

displayPath :: Grid -> Path -> Text
displayPath g s = displayG $ fmap showInGrid g `Map.union` Map.fromList g'
  where
    g' = [(p, showInGrid d) | Deer p d <- s]

tasks =
  Tasks
    2024
    16
    (CodeBlock 0)
    parser
    [ task part1 7036 & taskPart 1
    , task part1 11048 & taskScraper (CodeBlock 2)
    , task part2 45 & taskPart 2
    , task part2 64 & taskScraper (CodeBlock 2)
    ]
