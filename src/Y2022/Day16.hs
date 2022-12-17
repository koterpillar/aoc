{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Path
import           Utils

import           Y2022.Lattice

type VKey = Text

data VData =
  VData
    { vFlow :: Int
    , vNext :: [VKey]
    }
  deriving (Eq, Ord, Show)

type Input = Map VKey VData

iValves :: Input -> [VKey]
iValves = mapFilterValues $ (> 0) . vFlow

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
    , vPositions :: [VKey]
    , vPrevs     :: [Maybe VKey]
    , vOpenK     :: Set VKey
    , vMinute    :: Int
    , vTotalTime :: Int
    , vReleased  :: Int
    }
  deriving (Eq, Ord)

vAt :: VState -> VKey -> VData
vAt VState {..} k = fromJustE "vAt" $ Map.lookup k vMap

vPosition :: Int -> VState -> VKey
vPosition i VState {..} = vPositions !! i

vPrev :: Int -> VState -> Maybe VKey
vPrev i VState {..} = vPrevs !! i

vHere :: Int -> VState -> VData
vHere i s = vAt s (vPosition i s)

instance Show VState where
  show s =
    "T" ++
    show (vMinute s) ++
    " at " ++
    show (vPositions s) ++
    " open " ++
    show (Set.toList $ vOpenK s) ++
    " released " ++
    show (vReleased s) ++
    " current " ++ show (vCurrentFlow s) ++ " total " ++ show (vTotalFlow s)

vOpen :: VState -> [VData]
vOpen s = map (vAt s) $ Set.toList (vOpenK s)

vCurrentFlow :: VState -> Int
vCurrentFlow = sum . map vFlow . vOpen

vTotalFlow :: VState -> Int
vTotalFlow s = vReleased s + (vTotalTime s - vMinute s) * vCurrentFlow s

vIsOpen :: VKey -> VState -> Bool
vIsOpen k s = k `Set.member` vOpenK s

vTurn :: Int -> VState -> Maybe VState
vTurn i s
  | vIsOpen (vPosition i s) s = Nothing
  | vFlow (vHere i s) == 0 = Nothing
  | otherwise =
    Just $
    s
      { vOpenK = Set.insert (vPosition i s) (vOpenK s)
      , vPrevs = sset i Nothing $ vPrevs s
      }

vWalk :: Int -> VState -> VKey -> VState
vWalk i s k =
  s
    { vPositions = sset i k $ vPositions s
    , vPrevs = sset i (Just $ vPosition i s) $ vPrevs s
    }

vTargetFlow :: VState -> VKey -> VKey -> Int
vTargetFlow s k1 k2 =
  if vFlow p2 > 0 && not (vIsOpen k2 s)
    then vFlow p2
    else case filter (/= k1) $ vNext p2 of
           [k3] -> vTargetFlow s k2 k3
           _    -> 0
  where
    p2 = vAt s k2

vReachable :: Int -> VState -> [VKey]
vReachable i s = sorted
  where
    nexts = [k | k <- vNext p1, Just k /= vPrev i s]
    sorted = sortOn (negate . vTargetFlow s k1) nexts
    p1 = vAt s k1
    k1 = vPosition i s

vMoves :: VState -> [VState]
vMoves s
  | vMinute s == vTotalTime s = []
  | otherwise = map tick $ foldr mi [s] [0 .. length (vPositions s) - 1]
  where
    mi i =
      concatMap $ \s_ ->
        maybeToList (vTurn i s_) ++ map (vWalk i s_) (vReachable i s)
    tick s_ = s_ {vMinute = succ $ vMinute s_, vReleased = flow}
    flow = vReleased s + sum (map vFlow (vOpen s))

vInit :: Int -> Int -> Input -> VState
vInit minutes workers vMap = VState {..}
  where
    vPositions = replicate workers "AA"
    vPrevs = replicate workers Nothing
    vOpenK = mempty
    vMinute = 0
    vTotalTime = minutes
    vReleased = 0

instance Positionable VState where
  latticePosition s =
    map valve (iValves $ vMap s) ++ [60 - vMinute s, vReleased s]
    where
      valve k =
        if vIsOpen k s
          then 1
          else 0

type Seen = Map [VKey] Lattice

initSeen :: Seen
initSeen = mempty

checkMarkSeen :: VState -> State Seen Bool
checkMarkSeen a = do
  let k = vPositions a
  bs <- gets $ fromMaybe latticeEmpty . Map.lookup k
  case latticeInsert bs a of
    Nothing -> pure False
    Just bs' -> do
      traceM $ prependShow "improvement" a
      modify' (Map.insert k bs')
      pure True

dfs :: VState -> State Seen [VState]
dfs a =
  checkMarkSeen a >>= \case
    False -> pure []
    True  -> fmap ((a :) . concat) <$> traverse dfs $ vMoves a

start :: Int -> Int -> Input -> Int
start minutes workers input =
  vTotalFlow $ traceShowId $ maximumOn vTotalFlow $ evalState (dfs st) initSeen
  where
    st = vInit minutes workers input

part1 = start 30 1

part2 = start 26 2

-- on my data the right answer to part 2 is between 2722 and 2922
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651, Task part2 1707]
