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
  | Open Position2 Int
  | OpenStep Direction4
  deriving (Show, Eq, Ord)

giPosition :: GI -> Position2
giPosition (Open p _) = p
giPosition gi         = error $ "giPosition: " ++ show gi

giDirection :: GI -> Direction4 -> Direction4
giDirection (Open _ d) = iterateNL d turnLeft

instance GridItem GI where
  showInGrid Wall         = '#'
  showInGrid Open {}      = 'o'
  showInGrid (OpenStep d) = showInGrid d

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
    wp p Open0  = Just $ Open p 0
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

giProjectBack :: GI -> You -> You
giProjectBack c (You p d f) = You (giPosition c) (giDirection c d) f

gProjectBack :: IsGrid grid => grid -> You -> You
gProjectBack g you = giProjectBack (fromJustE "gProjectBack" $ gAt g you) you

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

yScore :: You -> Int
yScore (You p d _) = score p d

doWalksScore ::
     IsGrid grid => (grid -> You -> You) -> grid -> [Instruction] -> You -> Int
doWalksScore stp g is you = ttrace (displayG g') $ yScore $ head path
  where
    path = map (gProjectBack g) $ doWalks stp g is you
    g' = foldr putStep (gOriginal g) path
    putStep (You p d _) = Map.insert p (OpenStep d)

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

rotgridR :: Grid2 GI -> Grid2 GI
rotgridR g = Map.fromList $ map go $ Map.toList g
  where
    go (Position2 x y, c) = (Position2 (d - y) x, go1 c)
    go1 (Open p d) = Open p (succ d)
    go1 c          = c
    (Position2 xmin _, Position2 xmax _) = boundsG g
    d = xmax - xmin

rotgridUD :: Grid2 GI -> Grid2 GI
rotgridUD = rotgridR . rotgridR

rotgridL :: Grid2 GI -> Grid2 GI
rotgridL = rotgridR . rotgridR . rotgridR

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
jump sz FBack E  = edgeDown sz FRight N
jump sz FBack W  = edgeUp sz FLeft N
jump sz FBack S  = edgeUp sz FDown N
jump sz FBack N  = edgeUp sz FUp S
jump sz FFront E = edgeUp sz FRight S
jump sz FFront W = edgeDown sz FLeft S
jump sz FFront S = edgeUp sz FUp N
jump sz FFront N = edgeUp sz FDown S
jump sz FRight E = edgeDown sz FUp E
jump sz FRight W = edgeUp sz FDown E
jump sz FRight S = edgeUp sz FFront E
jump sz FRight N = edgeDown sz FBack E
jump sz FLeft E  = edgeUp sz FDown W
jump sz FLeft W  = edgeDown sz FUp W
jump sz FLeft S  = edgeDown sz FFront W
jump sz FLeft N  = edgeUp sz FBack W
jump sz FDown E  = edgeUp sz FRight W
jump sz FDown W  = edgeUp sz FLeft E
jump sz FDown S  = edgeUp sz FFront N
jump sz FDown N  = edgeUp sz FBack S
jump sz FUp E    = edgeDown sz FRight E
jump sz FUp W    = edgeDown sz FLeft W
jump sz FUp S    = edgeUp sz FBack N
jump sz FUp N    = edgeUp sz FFront S

edgeUp :: Int -> Face -> Direction4 -> Int -> You
edgeUp sz f N c = You (Position2 c 0) S f
edgeUp sz f S c = You (Position2 c (pred sz)) N f
edgeUp sz f W c = You (Position2 0 c) E f
edgeUp sz f E c = You (Position2 (pred sz) c) W f

edgeDown :: Int -> Face -> Direction4 -> Int -> You
edgeDown sz f d = edgeUp sz f d . flipp sz

flipp :: Int -> Int -> Int
flipp sz c = sz - c - 1

chunkify :: Grid2 a -> (Int, Grid2 (Grid2 a))
chunkify g =
  ( sz
  , Map.filter (not . null) $ fromMatrixG [[subgrid sz x y g | x <- p] | y <- p])
  where
    p = [0 .. 3]
    sz = round $ sqrt $ fromIntegral $ (`div` 6) $ length g

cubify :: Grid -> Grid3
cubify g = Grid3 {..}
  where
    _g2D = gOriginal g
    (_gSize, gs0) = chunkify _g2D
    gs = fixupExample gs0
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
    [task part1 6032, task testCubify 6, task part2 5031]
