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

newtype Layers =
  Layers (Map VKey KTree)

data KTree
  = Leaf VState
  | Stmp
  | Node VKey KTree KTree
  deriving (Eq, Ord, Show)

kempty :: KTree
kempty = Stmp

ktoList :: KTree -> [VState]
ktoList (Leaf v)     = [v]
ktoList (Node _ l r) = ktoList l ++ ktoList r

kinsert :: VState -> KTree -> KTree
kinsert v Stmp = Leaf v
kinsert v1 (Leaf v2) =
  case sampleDiff (vOpenK v1) (vOpenK v2) of
    Just k -> Node k (Leaf v2) (Leaf v1)
    Nothing ->
      case sampleDiff (vOpenK v2) (vOpenK v1) of
        Just k  -> Node k (Leaf v1) (Leaf v2)
        Nothing -> Leaf v1
kinsert v (Node k l r)
  | Set.member k (vOpenK v) = Node k l (kinsert v r)
  | otherwise = Node k (kinsert v l) r

-- | Returns an element that is present in first but not the second set, if one exists.
sampleDiff :: Ord a => Set a -> Set a -> Maybe a
sampleDiff a b = Set.lookupMin (Set.difference a b)

layers0 :: Layers
layers0 = Layers mempty

unLayers :: Layers -> [VState]
unLayers (Layers ls) = concatMap ktoList $ toList ls

layersSize :: Layers -> Int
layersSize (Layers ls) = length ls

kWorthy :: VState -> KTree -> Bool
kWorthy _ Stmp = True
kWorthy b (Leaf a) = vTotalFlow b > vTotalFlow a
kWorthy b (Node k l r)
  | Set.member k (vOpenK b) = kWorthy b l && kWorthy b r
  | otherwise = kWorthy b l

keep :: Layers -> VState -> Bool
keep (Layers ls) v = kWorthy v $ fromMaybe Stmp $ Map.lookup (vPosition v) ls

layerAdd :: VState -> Layers -> Layers
layerAdd v l@(Layers ls)
  | keep l v =
    Layers $ Map.alter (Just . kinsert v . fromMaybe kempty) (vPosition v) ls
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
    ns1 = traceF (prependShow "cmp" . length) $ compact ns0
    ns = ns1

start :: Int -> Int -> Input -> Int
start minutes workers input =
  vTotalFlow $ traceShowId $ maximumOn vTotalFlow $ unLayers $ lGo [st] layers0
  where
    st = vInit minutes workers $ traceShowId input

part1 = start 30 1

part2 = start 26 2

-- Warning: part1 requires >90 seconds but <600
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651]
