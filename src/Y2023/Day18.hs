module Y2023.Day18
  ( tasks
  ) where

import           Control.Parallel.Strategies (parMap, rpar)

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text

import           AOC
import           Grid
import           Utils

type Instr = (Direction4, Int)

mkInstr :: Direction4 -> Int -> Instr -> (Instr, Instr)
mkInstr d i hidden = ((d, i), hidden)

hiddenInstrP :: Parser Text Instr
hiddenInstrP =
  charactersP
    &* pureP (drop 1 . reverse . drop 2)
    &* unconsP
    &* (choiceEBP "0123" &= (pureP (Text.pack . reverse) &* hexP))

parser :: Parser Text [(Instr, Instr)]
parser =
  linesP
    &** (wordsP
           &* ap3P mkInstr (charP &* choiceEBP "RULD") integerP hiddenInstrP)

type Grid = Grid2 ()

data Fence = Fence
  { fP1 :: Position2
  , fP2 :: Position2
  } deriving (Show)

fMk :: Position2 -> Direction4 -> Int -> Fence
fMk p N l = Fence (walkN l N p) p
fMk p E l = Fence p (walkN l E p)
fMk p S l = Fence p (walkN l S p)
fMk p W l = Fence (walkN l W p) p

fEnds :: Fence -> [Position2]
fEnds (Fence p1 p2) = [p1, p2]

fApply :: (Position2 -> Position2) -> Fence -> Fence
fApply f (Fence p1 p2) = Fence (f p1) (f p2)

fPoints :: Fence -> [Position2]
fPoints (Fence (Position2 x1 y1) (Position2 x2 y2)) =
  [Position2 x y | x <- [x1 .. x2], y <- [y1 .. y2]]

fGrid :: [Fence] -> Grid
fGrid = void . mapFromListCount . concatMap fPoints

fDisplay :: [Fence] -> Text
fDisplay = displayG . fGrid

fences :: [Instr] -> [Fence]
fences = go (Position2 0 0)
  where
    go _ [] = []
    go p ((d, n):is) = fMk p d n : go p' is
      where
        p' = walkN n d p

data Shrink a = Shrink
  { shrinkMap    :: a -> a
  , shrinkFactor :: a -> Int
  }

shrinkAp :: (a -> b -> c) -> (c -> (a, b)) -> Shrink a -> Shrink b -> Shrink c
shrinkAp f g (Shrink ma fa) (Shrink mb fb) = Shrink mc fc
  where
    mc c =
      let (a, b) = g c
       in f (ma a) (mb b)
    fc c =
      let (a, b) = g c
       in fa a * fb b

shrink :: Set Int -> Shrink Int
shrink ns = Shrink m f
  where
    m :: Int -> Int
    m x =
      2 * length (Set.takeWhileAntitone (< x) ns)
        + (if Set.member x ns
             then 1
             else 0)
    f :: Int -> Int
    f x
      | odd x = 1
      | x <= 0 = 1
      | otherwise =
        case drop (x `div` 2 - 1) (Set.toList ns) of
          (n1:n2:_) -> n2 - n1 - 1
          _         -> 1

shrinkFences :: [Fence] -> (Shrink Position2, [Fence])
shrinkFences fs = (pShrink, fs')
  where
    ps = Set.fromList $ concatMap fEnds fs
    xs = Set.map pX ps
    ys = Set.map pY ps
    xShrink = shrink xs
    yShrink = shrink ys
    pShrink = shrinkAp Position2 (\(Position2 x y) -> (x, y)) xShrink yShrink
    fs' = map (fApply $ shrinkMap pShrink) fs

filledArea :: [Instr] -> Int
filledArea is = insides
  where
    fs = fences is
    (s, fss) = shrinkFences fs
    g = ttraceF displayG $ fGrid fss
    insides = floodFill (shrinkFactor s) g

floodFill :: (Position2 -> Int) -> Grid -> Int
floodFill area g = sum $ parMap rpar insideLine [ymin .. ymax]
  where
    (Position2 xmin ymin, Position2 xmax ymax) = traceShowId $ boundsG g
    insideLine :: Int -> Int
    insideLine y = go y xmin False False
    go :: Int -> Int -> Bool -> Bool -> Int
    go y x n s
      | x > xmax = 0
      | otherwise =
        (if n || n' || s || s'
           then area (Position2 x y)
           else 0)
          + go y (succ x) n' s'
      where
        n' =
          n
            /= (Map.member (Position2 x y) g
                  && Map.member (Position2 x (pred y)) g)
        s' =
          s
            /= (Map.member (Position2 x y) g
                  && Map.member (Position2 x (succ y)) g)

part1 :: [(Instr, Instr)] -> Int
part1 = filledArea . map fst

part2 :: [(Instr, Instr)] -> Int
part2 = filledArea . map snd

tasks =
  Tasks (AOC 2023 18) (CodeBlock 0) parser
    $ let s = shrink $ Set.fromList [2, 5, 6, 10]
       in [ Assert "shrink map" [0, 0, 1, 2, 2, 3, 5, 6, 6, 6, 7]
              $ map (shrinkMap s) [0 .. 10]
          , Assert "shrink factor" [1, 1, 2, 1, 0, 1, 3, 1]
              $ map (shrinkFactor s) [0 .. 7]
          ]
            ++ [ task part1 62 & taskPart 1
               , task part2 952408144115 & taskPart 2
               ]
