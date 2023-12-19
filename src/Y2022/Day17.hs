{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day17 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Grid
import           Utils

parser :: Parser Text [Direction4]
parser = pureP ttrim &* charactersP &** choiceP [('<', W), ('>', E)]

type Grid = Grid2 ()

type Rock = Grid

mkgrid :: [Position2] -> Grid
mkgrid = Map.fromList . map (, ())

rocks :: [Rock]
rocks =
  map
    mkgrid
    [ [Position2 x 0 | x <- [0 .. 3]]
    , [ Position2 1 1
      , Position2 1 0
      , Position2 1 2
      , Position2 0 1
      , Position2 2 1
      ]
    , [ Position2 0 2
      , Position2 1 2
      , Position2 2 2
      , Position2 2 1
      , Position2 2 0
      ]
    , [Position2 0 y | y <- [0 .. 3]]
    , [Position2 x y | x <- [0 .. 1], y <- [0 .. 1]]
    ]

type StKey = (Int, Grid, Int)

data St = St
  { _stWind    :: [Direction4]
  , _stWindInd :: Int
  , _stChamber :: Grid
  , _stRock    :: Maybe Rock
  , _stDropped :: Int
  , _stOffset  :: Int
  , _stPrev    :: Map StKey St
  } deriving (Show)

$(makeLenses ''St)

stKey :: St -> StKey
stKey St {..} = (_stWindInd, _stChamber, _stDropped `mod` length rocks)

putInChamber :: Rock -> State St ()
putInChamber rock = do
  let (Position2 rxmin rymin, Position2 rxmax rymax) = boundsG rock
  (Position2 cxmin cymin, Position2 cxmax cymax) <- gets $ boundsG . _stChamber
  let dx = cxmin + 2 - rxmin
  let dy = cymin - 4 - rymax
  let r' = Map.mapKeys (pointPlus (Position2 dx dy)) rock
  stRock .= Just r'

data GI
  = R
  | C
  deriving (Ord, Eq, Show)

instance GridItem GI where
  showInGrid R = '@'
  showInGrid C = '#'

displaySt :: St -> Text
displaySt St {..} =
  displayG
    $ foldr
        Map.union
        (Map.map (const C) _stChamber)
        (Map.map (const R) <$> _stRock)

stInit :: [Direction4] -> St
stInit _stWind = St {..}
  where
    _stWindInd = 0
    _stChamber = mkgrid [Position2 x 0 | x <- [1 .. 7]]
    _stRock = Nothing
    _stDropped = 0
    _stOffset = 0
    _stPrev = mempty

stHeight :: St -> Int
stHeight St {..} = -ymin + _stOffset
  where
    (Position2 _ ymin, _) = boundsG _stChamber

getWind :: State St Direction4
getWind = do
  wind <- use stWind
  i <- stWindInd <<%= succMod (length wind)
  pure $ wind !! i

tryMoveRock :: Direction4 -> State St Bool
tryMoveRock d = do
  r <- fromJustE "no rock to move" <$> use stRock
  let r' = Map.mapKeys (walk d) r
  let rk = Map.keysSet r'
  hitFloor <-
    not . Set.null . Set.intersection rk . Map.keysSet <$> use stChamber
  let hitWalls =
        let (Position2 xmin _, Position2 xmax _) = boundsG r'
         in xmin < 1 || xmax > 7
  let hitAnything = hitFloor || hitWalls
  if hitAnything
    then pure False
    else do
      stRock .= Just r'
      pure True

dropRockGo :: State St ()
dropRockGo = do
  getWind >>= tryMoveRock
  c <- tryMoveRock S
  if c
    then dropRockGo
    else do
      r <- use stRock
      stRock .= Nothing
      stChamber %= Map.union (fromJustE "no rock to merge" r)

dropRock :: State St ()
dropRock = do
  n <- use stDropped
  when (n `mod` 20 == 0) optimize
  let r = rocks !! (n `mod` length rocks)
  putInChamber r
  dropRockGo
  stDropped %= succ

probe :: Grid -> Int -> Int -> Int -> Maybe Int
probe g ymin ymax x =
  listToMaybe [y | y <- [ymin .. ymax], Map.member (Position2 x y) g]

optimize :: State St ()
optimize = do
  g <- use stChamber
  let (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
  for_ (traverse (probe g ymin ymax) [xmin .. xmax]) $ \depths -> do
    let cutoff = maximum depths
    let g' =
          Map.mapKeys (\(Position2 x y) -> Position2 x (y - cutoff))
            $ Map.filterWithKey (\(Position2 _ y) _ -> y <= cutoff) g
    stChamber .= g'
    stOffset %= subtract cutoff

dropRocks :: Int -> State St ()
dropRocks 0 = pure ()
dropRocks n = do
  dropRock
  k <- gets stKey
  v <- gets (Map.lookup k . _stPrev)
  case v of
    Nothing -> do
      s <- get
      stPrev %= Map.insert k s
      dropRocks $ pred n
    Just s0 -> do
      s1 <- get
      let period = _stDropped s1 - _stDropped s0
      let growth = stHeight s1 - stHeight s0
      let skip = n `div` period
      stDropped %= (+ skip * period)
      stOffset %= (+ skip * growth)
      dropRocks $ pred n - skip * period

start :: Int -> [Direction4] -> Int
start n = stHeight . execState (dropRocks n) . stInit

tasks =
  Tasks
    2022
    17
    (CodeBlock 1)
    parser
    [Task (start 2022) 3068, Task (start 1000000000000) 1514285714288]
