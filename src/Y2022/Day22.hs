{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day22 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Grid
import           Utils

data GI0
  = Wall0
  | Open0
  | Blank0
  deriving (Show, Eq, Ord, Bounded, Enum)

data GI
  = Wall
  | Open Position2
  | OpenStep Direction4
  deriving (Show, Eq, Ord)

giPosition :: GI -> Position2
giPosition Wall        = error "No position for wall"
giPosition (Open p)    = p
giPosition OpenStep {} = error "No position for OpenStep"

instance GridItem GI where
  showInGrid Wall         = '#'
  showInGrid Open {}      = 'o'
  showInGrid (OpenStep E) = '>'
  showInGrid (OpenStep W) = '<'
  showInGrid (OpenStep N) = '^'
  showInGrid (OpenStep S) = 'v'

itemP :: Parser Char GI0
itemP = choiceEBP "#. "

data Grid =
  Gr
    { _gGrid   :: Grid2 GI
    , _gBounds :: (Position2, Position2)
    }
  deriving (Eq, Ord, Show)

makeLenses ''Grid

data Face
  = FBack
  | FLeft
  | FRight
  | FFront
  | FUp
  | FDown
  deriving (Eq, Ord, Show)

data Grid3 =
  Grid3
    { _gGrids :: Map Face (Grid2 GI)
    , _g2D    :: Grid2 GI
    , _gSize  :: Int
    }
  deriving (Eq, Ord, Show)

makeLenses ''Grid3

class Show grid =>
      IsGrid grid
  where
  gOriginal :: grid -> Grid2 GI
  gAt :: grid -> You -> Maybe GI

instance IsGrid Grid where
  gOriginal = _gGrid
  gAt g (You p _ _) = g ^. gGrid . at p

instance IsGrid Grid3 where
  gOriginal = _g2D
  gAt g (You p _ f) = g ^. gGrids . at f . non Map.empty . at p

mkgrid :: [[GI0]] -> Grid
mkgrid g0 = Gr {..}
  where
    _gGrid = mapFilterMapWithKey wp $ fromMatrixG g0
    _gBounds = boundsG _gGrid
    wp _ Wall0  = Just Wall
    wp p Open0  = Just $ Open p
    wp _ Blank0 = Nothing

type Input = (Grid, [Instruction])

data Instruction
  = Forward Int
  | RotateLeft
  | RotateRight
  deriving (Ord, Eq, Show)

instructionsP :: Parser Text [Instruction]
instructionsP =
  pureP (Text.groupBy (\a b -> isDigit a == isDigit b)) &**
  (Forward <$> integerP) &|
  (RotateLeft <$ requireP "L") &|
  (RotateRight <$ requireP "R")

parser :: Parser Text Input
parser =
  lineGroupsP &*
  (fmap mkgrid (traverseP $ charactersP &** itemP) &+ (singleP &* instructionsP))

data You =
  You
    { _yPosition  :: Position2
    , _yDirection :: Direction4
    , _yFace      :: Face
    }
  deriving (Eq, Show)

makeLenses ''You

step :: Grid -> You -> You
step g (You p d f) = You (wrapWalk g d p) d f

wrapWalk1 :: Grid -> Direction4 -> Position2 -> Position2
wrapWalk1 g d p = Position2 (w xmin xmax x) (w ymin ymax y)
  where
    (Position2 xmin ymin, Position2 xmax ymax) = g ^. gBounds
    (Position2 x y) = walk d p
    w a b c
      | c > b = c - b + a - 1
      | c < a = c + b - a + 1
      | otherwise = c

wrapWalk :: Grid -> Direction4 -> Position2 -> Position2
wrapWalk g d p =
  if g ^. gGrid . at p' . to isJust
    then p'
    else wrapWalk g d p'
  where
    p' = wrapWalk1 g d p

start :: Grid -> You
start g = You p E FBack
  where
    p = wrapWalk g E topLeft
    (topLeft, _) = g ^. gBounds

facingScore :: Direction4 -> Int
facingScore E = 0
facingScore S = 1
facingScore W = 2
facingScore N = 3

score :: Position2 -> Direction4 -> Int
score (Position2 x y) d = 1000 * (y + 1) + 4 * (x + 1) + facingScore d

doWalksScore ::
     IsGrid grid => (grid -> You -> You) -> grid -> [Instruction] -> You -> Int
doWalksScore stp g is you = ttrace (displayG g') $ score (op you') d'
  where
    path = doWalks stp g is you
    you' = head path
    op y = giPosition $ fromJustE "doWalks" $ gAt g y
    d' = you' ^. yDirection
    g' = foldr putStep (gOriginal g) path
    putStep y@(You _ d _) = Map.insert (op y) (OpenStep d)

doWalks ::
     IsGrid grid
  => (grid -> You -> You)
  -> grid
  -> [Instruction]
  -> You
  -> [You]
doWalks stp g is you = execState (traverse_ move is) [you]
  where
    move :: Instruction -> State [You] ()
    move (Forward i) = replicateM_ i stp1
    move RotateLeft  = _head . yDirection %= turnLeft
    move RotateRight = _head . yDirection %= turnRight
    stp1 = do
      you <- gets head
      let you' = stp g you
      when (gAt g you' /= Just Wall) $ modify (you' :)

part1 :: Input -> Int
part1 (g, is) = doWalksScore step g is $ start g

subgrid :: Int -> Int -> Int -> Grid2 a -> Grid2 a
subgrid sz ix iy = Map.mapKeys shift . mapFilterMapWithKey go
  where
    go (Position2 x y) v = do
      let (ax, rx) = x `divMod` sz
      guard $ ax == ix
      let (ay, ry) = y `divMod` sz
      guard $ ay == iy
      pure v
    shift (Position2 x y) = Position2 (x - ix * sz) (y - iy * sz)

rotgridR :: Grid2 a -> Grid2 a
rotgridR g = Map.mapKeys (\(Position2 x y) -> Position2 (d - y) x) g
  where
    (Position2 xmin _, Position2 xmax _) = boundsG g
    d = xmax - xmin

rotgridUD :: Grid2 a -> Grid2 a
rotgridUD = rotgridR . rotgridR

rotgridL :: Grid2 a -> Grid2 a
rotgridL = rotgridR . rotgridR . rotgridR

chunksize :: Grid2 a -> Int
chunksize = round . sqrt . fromIntegral . (`div` 6) . length

part2 :: Input -> Int
part2 (g, is) = doWalksScore step3 g3 is you3
  where
    g3 = cubify g
    you2 = start g
    g3back = g3 ^?! gGrids . ix FBack
    p3 =
      fromJustE "p3" $
      mapFindValue (\gi -> giPosition gi == _yPosition you2) g3back
    you3 = you2 {_yPosition = p3}

fixupExample :: Grid2 a -> Grid2 a
fixupExample g
  | Map.member (Position2 1 0) g = g
  | otherwise = Map.mapKeys (\(Position2 x y) -> Position2 (pred x) y) g

step3 :: Grid3 -> You -> You
step3 g (You p d f)
  | x < 0 = j W y
  | x >= sz = j E y
  | y < 0 = j N x
  | y >= sz = j S x
  | otherwise = You p' d f
  where
    j = jump sz f
    sz = g ^. gSize
    p'@(Position2 x y) = walk d p

jump :: Int -> Face -> Direction4 -> Int -> You
jump sz FBack E c  = You (Position2 (flipp sz c) 0) S FRight
jump sz FBack W c  = You (Position2 c 0) S FLeft
jump sz FBack S c  = You (Position2 c 0) S FDown
jump sz FBack N c  = You (Position2 c (pred sz)) N FUp
jump sz FFront E c = You (Position2 (flipp sz c) (pred sz)) N FRight
jump sz FFront W c = You (Position2 c (pred sz)) N FLeft
jump sz FFront S c = You (Position2 c 0) S FUp
jump sz FFront N c = You (Position2 c (pred sz)) N FDown
jump sz FRight E c = You (Position2 (pred sz) (flipp sz c)) W FUp
jump sz FRight W c = You (Position2 (pred sz) c) W FDown
jump sz FRight S c = You (Position2 (pred sz) c) W FFront
jump sz FRight N c = You (Position2 (pred sz) (flipp sz c)) W FBack
jump sz FLeft E c  = You (Position2 0 c) E FDown
jump sz FLeft W c  = You (Position2 0 (flipp sz c)) E FUp
jump sz FLeft S c  = You (Position2 0 c) E FFront
jump sz FLeft N c  = You (Position2 0 (flipp sz c)) E FBack
jump sz FDown E c  = You (Position2 0 c) E FRight
jump sz FDown W c  = You (Position2 (pred sz) c) W FLeft
jump sz FDown S c  = You (Position2 c 0) S FFront
jump sz FDown N c  = You (Position2 c (pred sz)) N FBack
jump sz FUp E c    = You (Position2 (pred sz) (flipp sz c)) W FRight
jump sz FUp W c    = You (Position2 0 (flipp sz c)) E FLeft
jump sz FUp S c    = You (Position2 c 0) S FBack
jump sz FUp N c    = You (Position2 c (pred sz)) N FFront

flipp :: Int -> Int -> Int
flipp sz c = sz - c - 1

chunkify :: Grid2 a -> Grid2 (Grid2 a)
chunkify g =
  Map.filter (not . null) $ fromMatrixG [[subgrid sz x y g | x <- p] | y <- p]
  where
    p = [0 .. 3]
    sz = traceF (prependShow "sz") $ chunksize g

cubify :: Grid -> Grid3
cubify g = Grid3 {..}
  where
    _g2D = gOriginal g
    gs = fixupExample $ chunkify _g2D
    _gSize = chunksize $ snd $ Map.findMin gs
    _gGrids =
      Map.fromList
        [ e FBack [(id, Position2 1 0)]
        , e FDown [(id, Position2 1 1)]
        , e FLeft [(id, Position2 0 1), (rotgridR, Position2 0 2)]
        , e FRight
            [ (id, Position2 2 1)
            , (rotgridL, Position2 2 2)
            , (rotgridR, Position2 2 0)
            ]
        , e FFront [(id, Position2 1 2)]
        , e FUp
            [ (id, Position2 1 3)
            , (rotgridUD, Position2 (-1) 1)
            , (rotgridL, Position2 0 3)
            ]
        ]
    e f as =
      case catMaybes [fn <$> Map.lookup p gs | (fn, p) <- as] of
        [] ->
          error $ traceInput $ "Cannot find " ++ show f ++ " in " ++
          show (Map.keys gs)
        r:_ -> (f, r)
    traceInput = ttrace (displayG $ Map.map (const ()) gs)

testCubify :: Input -> Int
testCubify = length . _gGrids . cubify . fst

tasks =
  Tasks
    2022
    22
    (CodeBlock 0)
    parser
    [Task part1 6032, Task testCubify 6, Task part2 5031]
