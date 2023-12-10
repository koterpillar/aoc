module Y2023.Day08 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Cycle
import           Utils

data Dir2
  = L
  | R
  deriving (Show, Eq, Ord, Bounded, Enum)

data Place =
  Place
    { pLeft  :: Text
    , pRight :: Text
    }
  deriving (Show, Eq, Ord)

data CamelMap =
  CamelMap
    { cInstructions :: [Dir2]
    , cPlaces       :: Map Text Place
    }
  deriving (Show, Eq, Ord)

placeP :: Parser Text Place
placeP =
  pureP (Text.drop 1 . Text.dropEnd 1) &* tsplitP ", " &* ap2P Place idP idP

parser :: Parser Text CamelMap
parser =
  lineGroupsP &*
  ap2P
    CamelMap
    (singleP &* charactersP &** choiceEBP "LR")
    (Map.fromList <$> traverseP (tsplitP " = " &* (idP &+ placeP)))

cFollow :: Text -> Dir2 -> CamelMap -> Text
cFollow p d cm =
  let Place l r = cPlaces cm Map.! p
   in if d == L
        then l
        else r

type St = (Text, Int)

cMove :: CamelMap -> St -> St
cMove cm (p, l) =
  ( cFollow p (cInstructions cm !! l) cm
  , (l + 1) `mod` length (cInstructions cm))

cWin1, cWin2 :: St -> Bool
cWin1 (p, _) = p == "ZZZ"

cWin2 (p, _) = Text.takeEnd 1 p == "Z"

cStart1 :: St
cStart1 = ("AAA", 0)

cStarts2 :: CamelMap -> [St]
cStarts2 cm = [(p, 0) | p <- Map.keys (cPlaces cm), Text.takeEnd 1 p == "A"]

untilTrue :: Cycle Bool -> Int
untilTrue = length . takeWhile not . cycleGenerate

part1 cm = untilTrue $ cycleMap cWin1 $ cycleFind (cMove cm) cStart1

part2 cm =
  untilTrue $
  traceShowId $
  foldl1 (cycleMergeWith (&&)) $
  traceShowId $ map (cycleMap cWin2 . cycleFind (cMove cm)) $ cStarts2 cm

tasks =
  Tasks
    2023
    8
    (CodeBlock 0)
    parser
    [ Task part1 2
    , TaskScraper (CodeBlock 1) part1 6
    , TaskScraper (CodeBlock 2) part2 6
    ]
