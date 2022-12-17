{-# LANGUAGE Strict #-}

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

putInChamber :: Rock -> State St ()
putInChamber rock = do
  let (Position2 rxmin rymin, Position2 rxmax rymax) = boundsG rock
  (Position2 cxmin cymin, Position2 cxmax cymax) <- gets $ boundsG . stChamber
  let dx = cxmin + 2 - rxmin
  let dy = cymin - 4 - rymax
  let r' = Map.mapKeys (pointPlus (Position2 dx dy)) rock
  modify $ \s -> s {stRock = Just r'}

data St =
  St
    { stWind    :: [Direction4]
    , stWindInd :: Int
    , stChamber :: Grid
    , stRock    :: Maybe Rock
    , stDropped :: Int
    , stOffset  :: Int
    , stPrev    :: Map StKey St
    }
  deriving (Show)

type StKey = (Int, Grid, Int)

stKey :: St -> StKey
stKey St {..} = (stWindInd, stChamber, stDropped `mod` length rocks)

data GI
  = R
  | C
  deriving (Ord, Eq, Show)

instance GridItem GI where
  showInGrid R = '@'
  showInGrid C = '#'

displaySt :: St -> Text
displaySt St {..} =
  displayG $
  foldr Map.union (Map.map (const C) stChamber) (Map.map (const R) <$> stRock)

succMod :: Int -> Int -> Int
succMod n x = succ x `mod` n

stInit :: [Direction4] -> St
stInit stWind = St {..}
  where
    stWindInd = 0
    stChamber = mkgrid [Position2 x 0 | x <- [1 .. 7]]
    stRock = Nothing
    stDropped = 0
    stOffset = 0
    stPrev = mempty

stHeight :: St -> Int
stHeight St {..} = -ymin + stOffset
  where
    (Position2 _ ymin, _) = boundsG stChamber

getWind :: State St Direction4
getWind = do
  wind <- gets stWind
  i <- gets stWindInd
  modify $ \s -> s {stWindInd = succMod (length wind) i}
  pure $ wind !! i

tryMoveRock :: Direction4 -> State St Bool
tryMoveRock d = do
  r <- gets $ fromJustE "no rock to move" . stRock
  let r' = Map.mapKeys (walk d) r
  let rk = Map.keysSet r'
  hitFloor <-
    gets $ not . Set.null . Set.intersection rk . Map.keysSet . stChamber
  let hitWalls =
        let (Position2 xmin _, Position2 xmax _) = boundsG r'
         in xmin < 1 || xmax > 7
  let hitAnything = hitFloor || hitWalls
  if hitAnything
    then pure False
    else do
      modify $ \s -> s {stRock = Just r'}
      pure True

dropRockGo :: State St ()
dropRockGo = do
  getWind >>= tryMoveRock
  c <- tryMoveRock S
  if c
    then dropRockGo
    else modify $ \s ->
           s
             { stRock = Nothing
             , stChamber =
                 Map.union
                   (fromJustE "no rock to merge" $ stRock s)
                   (stChamber s)
             }

dropRock :: State St ()
dropRock = do
  n <- gets stDropped
  when (n `mod` 20 == 0) optimize
  let r = rocks !! (n `mod` length rocks)
  putInChamber r
  dropRockGo
  modify $ \s -> s {stDropped = succ n}

probe :: Grid -> Int -> Int -> Int -> Maybe Int
probe g ymin ymax x =
  listToMaybe [y | y <- [ymin .. ymax], Map.member (Position2 x y) g]

optimize :: State St ()
optimize = do
  g <- gets stChamber
  let (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
  for_ (traverse (probe g ymin ymax) [xmin .. xmax]) $ \depths -> do
    let cutoff = maximum depths
    let g' =
          Map.mapKeys (\(Position2 x y) -> Position2 x (y - cutoff)) $
          Map.filterWithKey (\(Position2 _ y) _ -> y <= cutoff) g
    modify $ \s -> s {stChamber = g', stOffset = stOffset s - cutoff}

dropRocks :: Int -> State St ()
dropRocks 0 = pure ()
dropRocks n = do
  dropRock
  k <- gets stKey
  v <- gets (Map.lookup k . stPrev)
  case v of
    Nothing -> do
      modify $ \s -> s {stPrev = Map.insert (stKey s) s (stPrev s)}
      dropRocks $ pred n
    Just s0 -> do
      s1 <- get
      let period = stDropped s1 - stDropped s0
      let growth = stHeight s1 - stHeight s0
      let skip = n `div` period
      modify $ \s ->
        s
          { stDropped = stDropped s + skip * period
          , stOffset = stOffset s + skip * growth
          }
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
