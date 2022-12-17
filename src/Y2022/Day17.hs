module Y2022.Day17 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

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

initChamber :: Grid
initChamber = mkgrid [Position2 x 0 | x <- [1 .. 7]]

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
    , stChamber :: Grid
    , stRock    :: Maybe Rock
    }

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

stInit :: [Direction4] -> St
stInit wind = St (cycle wind) initChamber Nothing

stHeight :: St -> Int
stHeight St {..} = ymax - ymin
  where
    (Position2 _ ymin, Position2 _ ymax) = boundsG stChamber

getWind :: State St Direction4
getWind = do
  wind <- gets stWind
  modify $ \s -> s {stWind = tail wind}
  return $ head wind

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

dropRock :: Rock -> State St ()
dropRock r = putInChamber r >> dropRockGo

start :: Int -> [Direction4] -> Int
start n input = stHeight st1
  where
    st0 = stInit input
    rs = take n $ cycle rocks
    act = traverse dropRock rs
    st1 = execState act st0

tasks =
  Tasks
    2022
    17
    (CodeBlock 1)
    parser
    [Task (start 2022) 3068, Task (start 1000000000000) 1514285714288]
