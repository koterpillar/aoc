module Y2023.Day12 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Pos
  = Op
  | Da
  | Uk
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Memoizable Pos where
  memoize f t = memoize (f . toEnum) (fromEnum t)

type Row = ([Pos], [Int])

parser :: Parser Text [Row]
parser =
  linesP &** (wordsP &* ((charactersP &** choiceEBP ".#?") &+ integersP ","))

possibilities :: [Pos] -> [Int] -> Int
possibilities = memoFix3 possibilities' False

rz :: [Int] -> [Int]
rz = dropWhile (== 0)

possibilities' ::
     (Bool -> [Pos] -> [Int] -> Int) -> Bool -> [Pos] -> [Int] -> Int
---
possibilities' _ False [] []        = 1
possibilities' _ True [] [0]        = 1
possibilities' _ _ [] _             = 0
---
possibilities' f v (Uk:r) c         = f v (Op : r) c + f v (Da : r) c
---
possibilities' f False (Op:r) c     = f False r c
possibilities' f True (Op:r) (0:c)  = f False r c
possibilities' _ True (Op:_) _      = 0
---
possibilities' f False (Da:r) c     = f True (Da : r) c
possibilities' f True (Da:r) []     = 0
possibilities' f True (Da:r) (0:_)  = 0
possibilities' f True (Da:r) (c:cs) = f True r (pred c : cs)

part1 = sum . map (uncurry possibilities)

fivetimes :: [Pos] -> [Pos]
fivetimes = intercalate [Uk] . replicate 5

part2 = part1 . map (fivetimes *** join . replicate 5)

tasks =
  Tasks
    2023
    12
    (CodeBlock 1)
    parser
    [ Assert "###." 1 (possibilities [Da, Da, Da, Op] [3])
    , Assert "##" 1 (possibilities [Da, Da] [2])
    , Assert ".##." 0 (possibilities [Op, Da, Da, Op] [3])
    , Assert "?##." 1 (possibilities [Uk, Da, Da, Op] [3])
    , AssertExample "second line" 4 (uncurry possibilities . head . tail)
    , Task part1 21
    , Task part2 525152
    ]
