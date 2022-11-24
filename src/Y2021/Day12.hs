module Y2021.Day12 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Path
import           Utils

data Cave
  = Start
  | End
  | Big String
  | Small String
  deriving (Ord, Eq, Show)

caveP :: Parser Text Cave
caveP =
  Parser $ \case
    "start" -> Right Start
    "end" -> Right End
    c
      | Text.all isUpper c -> Right $ Big $ Text.unpack c
      | Text.all isLower c -> Right $ Small $ Text.unpack c
      | otherwise -> Left ("caveP: " ++ Text.unpack c)

type Caves = Map Cave (Set Cave)

addLink :: Cave -> Cave -> Caves -> Caves
addLink c1 c2 = go c1 c2 . go c2 c1
  where
    go ca cb = Map.insertWith mappend ca (Set.singleton cb)

parseCaves :: Parser Text Caves
parseCaves =
  foldr (uncurry addLink) mempty <$>
  linesP &** (tsplitP "-" &* (caveP &+ caveP))

type CTNode = ([Cave], Bool)

type CTEdge = Cave

type CaveTree = Tree CTEdge CTNode

ctMoves :: Caves -> CTNode -> [CTEdge]
ctMoves _ ([], _)      = error "ctMoves: empty path"
ctMoves caves (c:_, _) = toList $ fromMaybe mempty $ mapLookup c caves

ctApply :: Cave -> CTNode -> Maybe CTNode
ctApply Start _ = Nothing
ctApply _ (End:_, _) = Nothing
ctApply c@(Small _) (cs, magic)
  | c `elem` cs && magic = Just (c : cs, False)
  | c `elem` cs = Nothing
  | otherwise = Just (c : cs, magic)
ctApply c (cs, magic) = Just (c : cs, magic)

countEnds :: CaveTree -> Int
countEnds (Tree (End:_, _) _) = 1
countEnds (Tree _ cs)         = sum $ map (countEnds . snd) cs

solve :: Bool -> Caves -> Int
solve magic caves =
  countEnds $ moveTree (ctMoves caves) ctApply ([Start], magic)

part1 = solve False

part2 = solve True

tasks = Tasks 2021 12 (CodeBlock 0) parseCaves [Task part1 10, Task part2 36]
