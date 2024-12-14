module Y2023.Day08
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Dir2
  = L
  | R
  deriving (Show, Eq, Ord, Bounded, Enum)

data Place = Place
  { pLeft  :: Text
  , pRight :: Text
  } deriving (Show, Eq, Ord)

data CamelMap = CamelMap
  { cInstructions :: [Dir2]
  , cPlaces       :: Map Text Place
  } deriving (Show, Eq, Ord)

placeP :: Parser Text Place
placeP =
  pureP (Text.drop 1 . Text.dropEnd 1) &* tsplitP ", " &* ap2P Place idP idP

parser :: Parser Text CamelMap
parser =
  lineGroupsP
    &* ap2P
         CamelMap
         (singleP &* charactersP &** choiceEBP "LR")
         (Map.fromList <$> traverseP (tsplitP " = " &* (idP &+ placeP)))

cFollow :: Text -> Dir2 -> CamelMap -> Text
cFollow p d cm =
  let Place l r = cPlaces cm Map.! p
   in if d == L
        then l
        else r

cInstruction :: Int -> CamelMap -> Dir2
cInstruction i cm = cInstructions cm !! (i `mod` length (cInstructions cm))

looper :: (Ord st, Show st) => (st -> st) -> (st -> Bool) -> st -> [Int]
looper next win = go 0
  where
    go i s
      | win s = i : go (i + 1) (next s)
      | otherwise = go (i + 1) (next s)

cMove :: CamelMap -> (Text, Int) -> (Text, Int)
cMove cm (p, l) = (cFollow p (cInstruction l cm) cm, l + 1)

cWin1 :: (Text, Int) -> Bool
cWin1 (p, _) = p == "ZZZ"

data Period = Period
  { pStart :: Int
  , pStep  :: Int
  } deriving (Show, Eq, Ord)

pOf :: [Int] -> Period
pOf (x:y:_) = Period x (y - x)

pGenerate :: Period -> [Int]
pGenerate (Period s st) = [s,s + st ..]

pHits :: Int -> Period -> Bool
pHits i (Period s st) = (i - s) `mod` st == 0

pMerge :: Period -> Period -> Period
pMerge p1 p2 = Period s st
  where
    s = fromJustE "find what hits" $ find (`pHits` p1) $ pGenerate p2
    st = lcm (pStep p1) (pStep p2)

part1 cm = pStart $ pOf $ looper (cMove cm) cWin1 ("AAA", 0)

cStarts2 cm = [(p, 0) | p <- Map.keys (cPlaces cm), Text.takeEnd 1 p == "A"]

cWin2 (p, _) = Text.takeEnd 1 p == "Z"

part2 cm =
  pStart
    $ traceShowId
    $ foldl1 pMerge
    $ traceShowId
    $ map (pOf . looper (cMove cm) cWin2)
    $ cStarts2 cm

tasks =
  Tasks
    2023
    8
    (CodeBlock 0)
    parser
    [ task part1 2
    , task part1 6 & taskScraper (CodeBlock 1)
    , task part2 6 & taskScraper (CodeBlock 2)
    ]
