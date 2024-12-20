module Y2024.Day20
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
  deriving (Eq, Ord, Show, Enum, Bounded)

instance GridItem Item where
  showInGrid Wall  = '#'
  showInGrid Start = 'S'
  showInGrid End   = 'E'

type Grid = Grid2 Item

parser :: Parser Text Grid
parser = charGridP

isExample :: Grid -> Bool
isExample g =
  let (p1, p2) = boundsG g
      sz = 14
   in traceShowId (p2 `pointMinus` p1) == Position2 sz sz

gStart :: Grid -> Position2
gStart = mapFindValueE "gStart" (== Start)

gEnd :: Grid -> Position2
gEnd = mapFindValueE "gEnd" (== End)

gMoves :: Grid -> Position2 -> [Position2]
gMoves g p =
  filter (\p' -> Map.lookup p' g /= Just Wall) [walk d p | d <- allDir4]

noCheats :: Grid -> Int
noCheats g =
  length
    $ fromJustE "noCheats a*"
    $ aStarDepthGoal (gMoves g) (manhattanDistance $ gEnd g)
    $ gStart g

-- walking from start and end simultaneously
data Cheater = Cheater
  { cFwd :: Position2
  , cRev :: Position2
  } deriving (Ord, Eq, Show)

instance Hashable Cheater where
  hashWithSalt s (Cheater a b) = hashWithSalt s (a, b)

cMk :: Grid -> Cheater
cMk = Cheater <$> gStart <*> gEnd

cWin :: Forbidden -> Grid -> Cheater -> Bool
cWin f g c = manhattanDistance (cFwd c) (cRev c) == 2 && Set.notMember c f

cEstimate :: Cheater -> Int
cEstimate = manhattanDistance <$> cFwd <*> cRev

type Forbidden = Set Cheater

cMoves :: Grid -> Cheater -> [Cheater]
cMoves g (Cheater a b) =
  (Cheater <$> gMoves g a <*> pure b) <|> (Cheater a <$> gMoves g b)

withCheats :: Forbidden -> Grid -> Maybe (Int, Cheater) -- returns length and cheat position
withCheats f g = do
  path <- aStarDepth (cMoves g) cEstimate (cWin f g) (cMk g)
  pure (length path + 2, lastE "withCheats last" path)

part1 :: Grid -> Int
part1 g = go Set.empty
  where
    noCheatTime = noCheats g
    isE = isExample g
    cutoff =
      if isE
        then 1
        else 100
    go :: Forbidden -> Int
    go f =
      case withCheats f g of
        Just (time, cheat) ->
          let saved = noCheatTime - time
           in if saved < cutoff
                then traceShow ("cut", saved) 0
                else traceShow ("cnt", saved, cheat) $ succ $ go $ Set.insert cheat f
        Nothing -> 0

tasks =
  Tasks
    2024
    20
    (CodeBlock 0)
    parser
    [ AssertExample "no cheats" 84 noCheats
    , task part1 (14 + 14 + 2 + 4 + 2 + 3 + 1 + 1 + 1 + 1 + 1)
        & taskPart 1
        & taskTimeout 900
    ]
