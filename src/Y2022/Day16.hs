{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Path
import           Utils

type VKey = Text

data VData =
  VData
    { vFlow :: Int
    , vNext :: [VKey]
    }
  deriving (Eq, Ord, Show)

type Input = Map VKey VData

parser :: Parser Text Input
parser =
  Map.fromList <$> linesP &** wordsP &* pureP tail &* unconsP &* (idP &= vdp)
  where
    vdp =
      pureP (drop 2) &*
      (uncurry VData <$>
       unconsP &*
       ((pureP (terase "rate=" . terase ";") &* integerP) &=
        (pureP (drop 4 . map (terase ",")) &* idP)))

data VState =
  VState
    { vMap       :: Input
    , vPosition  :: VKey
    , vOpenK     :: Set VKey
    , vMinute    :: Int
    , vTotalTime :: Int
    , vReleased  :: Int
    }
  deriving (Eq, Ord)

vAt :: VState -> VKey -> VData
vAt VState {..} k = fromJustE "vAt" $ Map.lookup k vMap

vHere :: VState -> VData
vHere s = vAt s (vPosition s)

instance Show VState where
  show s@VState {..} =
    "T" ++
    show vMinute ++
    " at " ++
    show vPosition ++
    " open " ++
    show (Set.toList vOpenK) ++
    " released " ++
    show vReleased ++
    " current " ++ show (vCurrentFlow s) ++ " total " ++ show (vTotalFlow s)

vOpen :: VState -> [VData]
vOpen s = map (vAt s) $ Set.toList (vOpenK s)

vCurrentFlow :: VState -> Int
vCurrentFlow = sum . map vFlow . vOpen

vTotalFlow :: VState -> Int
vTotalFlow s = vReleased s + (vTotalTime s - vMinute s) * vCurrentFlow s

vTurn :: VState -> Maybe VState
vTurn s
  | vPosition s `Set.member` vOpenK s = Nothing
  | vFlow (vHere s) == 0 = Nothing
  | otherwise = Just $ s {vOpenK = Set.insert (vPosition s) (vOpenK s)}

vWalk :: VState -> VKey -> VState
vWalk s k = s {vPosition = k}

vMoves :: VState -> [VState]
vMoves s
  | vMinute s == vTotalTime s = []
  | otherwise = map tick $ maybeToList (vTurn s) ++ map (vWalk s) reachable
  where
    tick s_ = s_ {vMinute = succ $ vMinute s_, vReleased = flow}
    flow = vReleased s + sum (map vFlow (vOpen s))
    reachable = vNext (vHere s)

vInit :: Int -> Int -> Input -> VState
vInit minutes workers vMap = VState {..}
  where
    vPosition = "AA"
    vOpenK = mempty
    vMinute = 0
    vTotalTime = minutes
    vReleased = 0

vNotBetter :: VState -> VState -> Bool
vNotBetter a b =
  vPosition a == vPosition b &&
  Set.isSubsetOf (vOpenK b) (vOpenK a) && vTotalFlow a >= vTotalFlow b

newtype Layers =
  Layers (Map (VKey, Set VKey) VState)

layers0 :: Layers
layers0 = Layers mempty

unLayers :: Layers -> [VState]
unLayers (Layers ls) = toList ls

layersSize :: Layers -> Int
layersSize (Layers ls) = length ls

keep :: Layers -> VState -> Bool
keep (Layers ls) v = not $ any (`vNotBetter` v) os
  where
    os =
      catMaybes
        [ Map.lookup (vPosition v, o) ls
        | o <- Set.toList (Set.powerSet (vOpenK v))
        ]

layerAdd :: VState -> Layers -> Layers
layerAdd v l@(Layers ls)
  | keep l v = Layers $ Map.insert (vPosition v, vOpenK v) v ls
  | otherwise = l

layerAddMany :: Layers -> [VState] -> Layers
layerAddMany = foldr layerAdd

layerMk :: [VState] -> Layers
layerMk = layerAddMany layers0

compact :: [VState] -> [VState]
compact = unLayers . layerMk

lGo :: [VState] -> Layers -> Layers
lGo l ls =
  if null ns
    then ls
    else lGo ns $ traceF (prependShow "lrs" . layersSize) $ layerAddMany ls l
  where
    l1 = traceF (prependShow "min" . vMinute . head) l
    ns0 = traceF (prependShow "raw" . length) $ concatMap vMoves l1
    ns2 = traceF (prependShow "cmp" . length) $ compact ns0
    ns = ns2

start :: Int -> Int -> Input -> Int
start minutes workers input =
  vTotalFlow $ traceShowId $ maximumOn vTotalFlow $ unLayers $ lGo [st] layers0
  where
    st = vInit minutes workers $ traceShowId input

part1 = start 30 1

part2 = start 26 2

-- Warning: part1 requires >90 seconds but <600
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651]
