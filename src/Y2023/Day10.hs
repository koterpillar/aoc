module Y2023.Day10 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Utils

data Pipe
  = PNS
  | PWE
  | PNE
  | PNW
  | PSE
  | PSW
  | PST
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Pipe where
  showInGrid PNS = '|'
  showInGrid PWE = '-'
  showInGrid PNE = 'L'
  showInGrid PNW = 'J'
  showInGrid PSE = 'F'
  showInGrid PSW = '7'
  showInGrid PST = 'S'

pEdges :: Pipe -> [Direction4]
pEdges PNS = [N, S]
pEdges PWE = [W, E]
pEdges PNE = [N, E]
pEdges PNW = [N, W]
pEdges PSE = [S, E]
pEdges PSW = [S, W]
pEdges PST = error "no edges for start"

pFromEdges :: [Direction4] -> Pipe
pFromEdges = go . sort
  where
    go [N, S] = PNS
    go [E, W] = PWE
    go [N, E] = PNE
    go [N, W] = PNW
    go [E, S] = PSE
    go [W, S] = PSW
    go es     = error $ "no pipe for edges" <> show es

pOtherEdge :: Direction4 -> Pipe -> Direction4
pOtherEdge d p = head $ filter (/= d) $ pEdges p

type Grid = Grid2 Pipe

parser :: Parser Text Grid
parser = charGridP

findStart :: Grid -> (Position2, Grid)
findStart grid = (start, Map.insert start (pFromEdges dirs) grid)
  where
    start = fromJustE "no start" $ findTuple (== PST) $ Map.toList grid
    dirs = filter d1 allDir4
      where
        d1 dir = reverse4 dir `elem` es
          where
            p1 = walk dir start
            r1 = mapLookup p1 grid
            es = maybe [] pEdges r1

type Points = Set Position2

findLoop :: Grid -> Points
findLoop grid = head $ mapMaybe (go (Set.singleton start) start) dirs
  where
    (start, grid') = findStart grid
    dirs = pEdges $ fromJustE "no start" $ mapLookup start grid'
    go :: Points -> Position2 -> Direction4 -> Maybe Points
    go l pos dir = do
      let pos' = walk dir pos
      r <- mapLookup pos' grid
      if r == PST
        then pure l
        else do
          let dir' = pOtherEdge (reverse4 dir) r
          go (Set.insert pos' l) pos' dir'

part1 :: Grid -> Int
part1 = (`div` 2) . succ . length . findLoop

countInside :: Points -> Grid -> Int
countInside loop grid = length $ insides loop grid

insides :: Points -> Grid -> Points
insides loop grid0 = foldl1 Set.union $ map scanLine [ymin .. ymax]
  where
    (_, grid) = findStart grid0
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG grid
    scanLine y = go xmin False False Set.empty
      where
        go x n s r
          | x > xmax = r
          | p `Set.member` loop = go (succ x) n' s' r
          | n && s = go (succ x) n s (Set.insert p r)
          | otherwise = go (succ x) n s r
          where
            p = Position2 x y
            es = maybe [] pEdges $ mapLookup p grid
            n' = n /= (N `elem` es)
            s' = s /= (S `elem` es)

part2 :: Grid -> Int
part2 g = countInside l g
  where
    l = findLoop g

tasks =
  Tasks
    2023
    10
    (CodeBlock 4)
    parser
    [ task part1 8
    , task part2 4 & taskScraper (CodeBlock 9)
    , task part2 8 & taskScraper (CodeBlock 12)
    , task part2 10 & taskScraper (CodeBlock 14)
    ]
