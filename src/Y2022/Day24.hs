{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day24 where

import qualified Data.Map as Map

import           AOC
import           Grid
import           Path
import           Utils

data Tile
  = Blizzard [Direction4]
  | Wall
  | You
  deriving (Ord, Eq, Show)

mergeTile :: Tile -> Tile -> Tile
mergeTile (Blizzard ds1) (Blizzard ds2) = Blizzard $ ds1 ++ ds2
mergeTile _ _                           = error "mergeTile"

instance GridItem Tile where
  showInGrid (Blizzard [d]) = showInGrid d
  showInGrid (Blizzard ds)  = showInGrid $ length ds
  showInGrid Wall           = '#'
  showInGrid You            = 'E'

type Grid = Grid2 Tile

tileP :: Parser Char Tile
tileP = choiceP $ ('#', Wall) : [(showInGrid d, Blizzard [d]) | d <- allDir4]

parser :: Parser Text Grid
parser = charGridP' tileP

allDir4None :: [Maybe Direction4]
allDir4None = Nothing : map Just allDir4

data Ctx =
  Ctx
    { _ctxPeriod :: Int
    , _ctxGrids  :: [Grid]
    , _ctxBounds :: (Position2, Position2)
    }
  deriving (Ord, Eq, Show)

makeLenses ''Ctx

type St = (Position2, Int)

stGrid :: Ctx -> Int -> Grid
stGrid ctx t = (ctx ^. ctxGrids) !! t

tick :: Ctx -> Int -> Int
tick ctx = succMod (ctx ^. ctxPeriod)

stDisplay :: Ctx -> St -> Text
stDisplay c s@(p, t) =
  tshow t <> "\n" <> displayG (Map.insert p You (stGrid c t))

ctxMake :: Grid -> Ctx
ctxMake g = Ctx {..}
  where
    _ctxBounds = boundsG g
    (pmin'@(Position2 xmin ymin), pmax'@(Position2 xmax ymax)) = _ctxBounds
    _ctxPeriod = lcm (xmax - xmin - 1) (ymax - ymin - 1)
    pmin = walk SE pmin'
    pmax = walk NW pmax'
    _ctxGrids = iterateN _ctxPeriod (moveBlizzards (pmin, pmax)) g

moveBlizzards :: (Position2, Position2) -> Grid -> Grid
moveBlizzards (pmin, pmax) g =
  mapFromListWith mergeTile $ concatMap (uncurry mt) $ Map.toList g
  where
    move d = wrapBounds (pmin, pmax) . walk d
    mt p Wall          = [(p, Wall)]
    mt p (Blizzard ds) = [(move d p, Blizzard [d]) | d <- ds]

ctxStart :: Ctx -> St
ctxStart c = (c ^. ctxBounds . _1 . walked E, 0)

ctxEnd :: SimpleGetter Ctx Position2
ctxEnd = ctxBounds . _2 . walked W

findRoute :: Ctx -> St -> Position2 -> [St]
findRoute c st end =
  fromJustE "findRoute" $
  aStarDepthGoal (moves c) (manhattanDistance end . fst) st

moves :: Ctx -> St -> [St]
moves c (p, t) = do
  let t' = tick c t
  d <- allDir4None
  let p' = walk d p
  guard $ insideBounds (c ^. ctxBounds) p'
  guard $ not $ Map.member p' $ stGrid c t'
  let r = (p', t')
  pure r

part1 g = length $ findRoute ctx (ctxStart ctx) (ctx ^. ctxEnd)
  where
    ctx = ctxMake g

part2 g = l1 + l2 + l3
  where
    ctx = ctxMake g
    rt st end =
      let r = findRoute ctx st end
       in (length r, lastE "part2" r)
    st0@(p1, _) = ctxStart ctx
    p2 = ctx ^. ctxEnd
    (l1, st1) = rt st0 p2
    (l2, st2) = rt st1 p1
    (l3, _) = rt st2 p2

tasks = Tasks 2022 24 (CodeBlock 6) parser [Task part1 18, Task part2 54]
