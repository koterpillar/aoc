{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day24 where

import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import           GHC.Generics (Generic)

import           AOC
import           Grid
import           Path
import           Utils

data Tile
  = Blizzard [Direction4]
  | Wall
  | You
  deriving (Ord, Eq, Show, Generic, Hashable)

mergeTile :: Tile -> Tile -> Tile
mergeTile (Blizzard ds1) (Blizzard ds2) = Blizzard $ ds1 ++ ds2
mergeTile _ _                           = error "mergeTile"

instance GridItem Tile where
  showInGrid (Blizzard [d]) = showInGrid d
  showInGrid (Blizzard ds)  = showInGrid $ length ds
  showInGrid Wall           = '#'
  showInGrid You            = 'E'

type Grid = Grid2 Tile

tileP :: Parser Char (Maybe Tile)
tileP =
  choiceP $ ('.', Nothing) : ('#', Just Wall) :
  [(showInGrid d, Just (Blizzard [d])) | d <- allDir4]

parser :: Parser Text Grid
parser = charGridMaybeP tileP

instance Walkable2 (Maybe Direction4) where
  walkN _ Nothing  = id
  walkN i (Just d) = walkN i d

allDir4None :: [Maybe Direction4]
allDir4None = Nothing : map Just allDir4

data Ctx =
  Ctx
    { _ctxPeriod :: Int
    , _ctxGrids  :: [Grid]
    , _ctxBounds :: (Position2, Position2)
    , _ctxEnd    :: Position2
    }
  deriving (Ord, Eq, Show, Generic, Hashable)

makeLenses ''Ctx

type St = (Position2, Int)

stGrid :: Ctx -> Int -> Grid
stGrid ctx t = (ctx ^. ctxGrids) !! t

tick :: Ctx -> Int -> Int
tick ctx = succMod (ctx ^. ctxPeriod)

succMod n x = succ x `mod` n

stDisplay :: Ctx -> St -> Text
stDisplay c s@(p, t) =
  tshow t <> "\n" <> displayG (Map.insert p You (stGrid c t))

ctxMake :: Grid -> Ctx
ctxMake g = trace (show _ctxPeriod) $ Ctx {..}
  where
    _ctxBounds = boundsG g
    (Position2 xmin ymin, Position2 xmax ymax) = _ctxBounds
    _ctxEnd = Position2 (xmax - 1) ymax
    _ctxPeriod = lcm (xmax - xmin - 1) (ymax - ymin - 1)
    _ctxGrids = iterateN _ctxPeriod moveBlizzards g

ctxStart :: Ctx -> St
ctxStart c = (walk E $ c ^. ctxBounds . _1, 0)

stDistanceToEnd :: Ctx -> St -> Int
stDistanceToEnd c = manhattanDistance (c ^. ctxEnd) . fst

findRoute :: Grid -> [St]
findRoute g =
  fromJustE "findRoute" $
  aStarDepthGoal (moves c) (stDistanceToEnd c) (ctxStart c)
  where
    c = ctxMake g

m1 :: Int -> Int -> Int -> Int
m1 a b c
  | c == a = b - 1
  | c == b = a + 1
  | otherwise = c

moveBlizzards :: Grid -> Grid
moveBlizzards g =
  mapFromListWith mergeTile $ concatMap (uncurry mt) $ Map.toList g
  where
    (Position2 xmin ymin, Position2 xmax ymax) = boundsG g
    move d p =
      let (Position2 x y) = walk d p
       in Position2 (m1 xmin xmax x) (m1 ymin ymax y)
    mt p Wall          = [(p, Wall)]
    mt p (Blizzard ds) = [(move d p, Blizzard [d]) | d <- ds]

moves :: Ctx -> St -> [St]
moves c (p, t) = do
  when (pX p `mod` 10 == 0) $ traceShowM p
  let t' = tick c t
  d <- allDir4None
  let p' = walk d p
  guard $ pY p' >= 0
  guard $ not $ Map.member p' $ stGrid c t'
  let r = (p', t')
  pure r

part1 = length . findRoute

tasks = Tasks 2022 24 (CodeBlock 6) parser [Task part1 18]
