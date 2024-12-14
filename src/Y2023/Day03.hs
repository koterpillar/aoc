module Y2023.Day03
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data GI
  = GN Int
  | GS Char
  deriving (Ord, Eq, Show)

parser :: Parser Text (Grid2 GI)
parser =
  charGridMaybeP
    $ (Just . GN <$> digitP) &| (Nothing <$ requireP '.') &| pureP (Just . GS)

numberAt :: Position2 -> Grid2 GI -> Maybe Int
numberAt p g =
  case Map.lookup p g of
    Just (GN n) -> Just n
    _           -> Nothing

type Number = (Int, [Position2])

numbers :: Grid2 GI -> [Number]
numbers g = map buildNumber $ filter numberStart $ Map.keys g
  where
    numberStart :: Position2 -> Bool
    numberStart p = isJust (numberAt p g) && isNothing (numberAt p' g)
      where
        p' = walk W p
    buildNumber :: Position2 -> Number
    buildNumber = buildNumberFrom (0, [])
    buildNumberFrom (n, ps) p =
      case numberAt p g of
        Just n' -> buildNumberFrom (n * 10 + n', p : ps) (walk E p)
        _       -> (n, ps)

symbolAdjacent :: Number -> Grid2 GI -> Bool
symbolAdjacent (_, ps) g =
  or $ do
    p <- ps
    d <- allDir8
    let p' = walk d p
    pure
      $ case Map.lookup p' g of
          Just (GS _) -> True
          _           -> False

part1 g = sum $ map fst $ filter (`symbolAdjacent` g) $ numbers g

part2 g = sum $ mapMaybe (gearRatio . grabNs) gps
  where
    ns = numbers g
    gps = Map.keys $ Map.filter (== GS '*') g
    grabNs gp = filter (adjacentTo gp) ns

adjacentTo :: Position2 -> Number -> Bool
adjacentTo p = any (`elem` ps) . snd
  where
    ps = adjacent8 p

gearRatio :: [Number] -> Maybe Int
gearRatio [(n1, _), (n2, _)] = Just $ n1 * n2
gearRatio _                  = Nothing

tasks = Tasks 2023 3 (CodeBlock 0) parser [Task part1 4361, Task part2 467835]
