{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import           Control.Monad.State.Strict

import           Data.Foldable              (foldrM)

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
    , vNext :: [(Int, VKey)]
    }
  deriving (Eq, Ord, Show)

mkVData :: Int -> [VKey] -> VData
mkVData f n = VData f $ zip (repeat 1) n

type Input = Map VKey VData

iValves :: Input -> [VKey]
iValves = mapFilterValues $ (> 0) . vFlow

parser :: Parser Text Input
parser =
  Map.fromList <$> linesP &** wordsP &* pureP tail &* unconsP &* (idP &= vdp)
  where
    vdp =
      pureP (drop 2) &*
      (uncurry mkVData <$>
       unconsP &*
       ((pureP (terase "rate=" . terase ";") &* integerP) &=
        (pureP (drop 4 . map (terase ",")) &* idP)))

data VPosition
  = VAt VKey
  | VEnRoute Int VKey
  deriving (Eq, Ord, Show)

vEnRoute :: Int -> VKey -> VPosition
vEnRoute 0 = VAt
vEnRoute i = VEnRoute i

data VState =
  VState
    { vMap       :: Input
    , vPositions :: [VPosition]
    , vOpenK     :: Set VKey
    , vMinute    :: Int
    , vTotalTime :: Int
    , vReleased  :: Int
    }
  deriving (Eq, Ord)

vAt :: VState -> VKey -> VData
vAt VState {..} k = fromJustE "vAt" $ Map.lookup k vMap

vPosition :: Int -> VState -> VPosition
vPosition i VState {..} = vPositions !! i

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
vTurn i s =
  case vPosition i s of
    VEnRoute {} -> Nothing
    VAt k       -> go k
  where
    go k
      | vIsOpen k s = Nothing
      | vFlow (vAt s k) == 0 = Nothing
      | otherwise = Just s {vOpenK = Set.insert k (vOpenK s)}

vWalk :: Int -> VState -> VPosition -> VState
vWalk i s k = s {vPositions = sset i k $ vPositions s}

vReachable :: Int -> VState -> [VPosition]
vReachable i s =
  case vPosition i s of
    VEnRoute i k -> [vEnRoute (i - 1) k]
    VAt k        -> [vEnRoute (i - 1) k' | (i, k') <- vNext $ vAt s k]

vIndices :: VState -> [Int]
vIndices s = [0 .. length (vPositions s) - 1]

vMoveOne :: Int -> VState -> [VState]
vMoveOne i s = maybeToList (vTurn i s) ++ map (vWalk i s) (vReachable i s)

vMoves :: VState -> [VState]
vMoves s0
  | vMinute s0 == vTotalTime s0 = []
  | otherwise = tick <$> foldrM vMoveOne s0 (vIndices s0)
  where
    tick s_ = s_ {vMinute = succ $ vMinute s_, vReleased = flow}
    flow = vReleased s0 + sum (map vFlow (vOpen s0))

vInit :: Int -> Int -> Input -> VState
vInit minutes workers vMap = VState {..}
  where
    vPositions = replicate workers $ VAt "AA"
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

type Seen = Map [VPosition] Lattice

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
