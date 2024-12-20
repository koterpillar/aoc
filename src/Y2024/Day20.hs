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

noCheats :: Grid -> Int
noCheats g =
  length
    $ fromJustE "noCheats a*"
    $ aStarDepthGoal moves (manhattanDistance $ gEnd g)
    $ gStart g
  where
    moves p =
      filter (\p' -> Map.lookup p' g /= Just Wall) [walk d p | d <- allDir4]

data Cheater = Cheater
  { cP     :: Position2
  , cStart :: Maybe Position2
  , cEnd   :: Maybe Position2
  } deriving (Ord, Eq, Show)

instance Hashable Cheater where
  hashWithSalt s (Cheater p a e) = hashWithSalt s (p, a, e)

cMk :: Grid -> Cheater
cMk g = Cheater (gStart g) Nothing Nothing

cWin :: Grid -> Cheater -> Bool
cWin g c = cP c == gEnd g

cCost :: Cheater -> Cheater -> Int
cCost = manhattanDistance `on` cP

cEstimate :: Grid -> Cheater -> Int
cEstimate g c = distance + startBonus + endBonus
  where
    distance = manhattanDistance (cP c) (gEnd g)
    startBonus
      | isJust (cStart c) = -1
      | otherwise = 0
    endBonus
      | isJust (cEnd c) = -1
      | otherwise = 0

type Cheat = (Position2, Position2)

type Forbidden = Set Cheat

cMoves :: Forbidden -> Grid -> Cheater -> [Cheater]
cMoves f g c
  | isJust (cEnd c) = plain -- already used
  | isJust (cStart c) = filter stillAllowed $ map setEnd plain -- get out of the wall and record
  | otherwise = plain ++ map setStart (filter (not . isFree . cP) around) -- get into the wall
  where
    around = [c {cP = walk d (cP c)} | d <- allDir4]
    plain = filter (isFree . cP) around
    isFree p = Map.lookup p g /= Just Wall
    setStart c' = c' {cStart = Just $ cP c'}
    setEnd c' = c' {cEnd = Just $ cP c'}
    stillAllowed c' =
      Set.notMember
        ( fromJustE "stillAllowedStart" $ cStart c'
        , fromJustE "stillAllowedEnd" $ cEnd c')
        f

withCheats :: Forbidden -> Grid -> (Int, Maybe Cheat) -- returns length and cheat position
withCheats f g = (length path, cheat)
  where
    path =
      fromJustE "withCheats a*" $ aStar (cMoves f g) cCost (cEstimate g) (cWin g) (cMk g)
    cheat = do
      c <- find (\c -> isJust (cStart c) && isJust (cEnd c)) path
      (,) <$> cStart c <*> cEnd c
    moves (p, 0) =
      filter (\p' -> Map.lookup p' g /= Just Wall) [walk d p | d <- allDir4]

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
      let (time, cheat) = withCheats f g
          saved = noCheatTime - time
       in if saved < cutoff
            then 0
            else case cheat of
                   Nothing -> traceShow ("end", saved) 0
                   Just c ->
                     traceShow ("cnt", saved) $ succ $ go $ Set.insert c f

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
