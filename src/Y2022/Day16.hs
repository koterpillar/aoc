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
    { vMap      :: Input
    , vPosition :: VKey
    , vOpenK    :: Set VKey
    , vMinute   :: Int
    , vReleased :: Int
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
vTotalFlow s = vReleased s + (minutes - vMinute s) * vCurrentFlow s

vReachable :: VState -> [VKey]
vReachable = vNext . vHere

vTick :: VState -> VState
vTick s = s {vMinute = succ $ vMinute s}

vTurn :: VState -> Maybe VState
vTurn s
  | vPosition s `Set.member` vOpenK s = Nothing
  | vFlow (vHere s) == 0 = Nothing
  | otherwise = Just $ s {vOpenK = Set.insert (vPosition s) (vOpenK s)}

vWalk :: VState -> VKey -> VState
vWalk s k = s {vPosition = k}

vMoves :: VState -> [VState]
vMoves s0 = map vTick $ maybeToList (vTurn s) ++ map (vWalk s) (vReachable s)
  where
    s = s0 {vReleased = vReleased s0 + sum (map vFlow (vOpen s0))}

vInit :: Input -> VState
vInit vMap = VState {..}
  where
    vPosition = "AA"
    vOpenK = mempty
    vMinute = 1
    vReleased = 0

vNotBetter :: VState -> VState -> Bool
vNotBetter a b =
  vPosition a == vPosition b &&
  Set.isSubsetOf (vOpenK b) (vOpenK a) && vReleased a >= vReleased b

type Layer = [VState]

newtype Layers =
  Layers Layer

lInit :: VState -> Layer
lInit v = [v]

layers0 :: Layers
layers0 = Layers []

unLayers :: Layers -> [VState]
unLayers (Layers ls) = ls

compact :: Layer -> Layer
compact []     = []
compact (v:vs) = v : compact (filter (not . vNotBetter v) vs)

keep :: Layers -> VState -> Bool
keep (Layers ls) v = not $ any (`vNotBetter` v) ls

layerAdd :: Layers -> Layer -> Layers
layerAdd (Layers ls) l = Layers (l ++ ls)

lGo :: Layer -> Layers -> Layers
lGo l ls =
  if null ns1
    then ls
    else lGo ns $ layerAdd ls l
  where
    l1 = traceShowF (("min", ) . vMinute . head) l
    m l
      | vMinute l == minutes = []
      | otherwise = vMoves l
    ns0 = traceShowF (("raw", ) . length) $ concatMap m l1
    ns1 = traceShowF (("cmp", ) . length) $ compact ns0
    ns2 = traceShowF (("kep", ) . length) $ filter (keep ls) ns1
    ns = ns2

minutes = 31

part1 input =
  vTotalFlow $
  traceShowId $ maximumOn vTotalFlow $ unLayers $ lGo (lInit st) layers0
  where
    st = vInit input

tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651]
