{-# LANGUAGE Strict #-}

module Y2022.Day16 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

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
  show s@VState {..} =
    "T" ++
    show vMinute ++
    " at " ++
    show vPositions ++
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

vTurn :: Int -> VState -> Maybe VState
vTurn i s
  | vPosition i s `Set.member` vOpenK s = Nothing
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

vMoves :: VState -> [VState]
vMoves s
  | vMinute s == vTotalTime s = []
  | otherwise = map tick $ foldr mi [s] [0 .. length (vPositions s) - 1]
  where
    mi i =
      concatMap $ \s_ ->
        maybeToList (vTurn i s_) ++ map (vWalk i s_) (reachable i)
    tick s_ = s_ {vMinute = succ $ vMinute s_, vReleased = flow}
    flow = vReleased s + sum (map vFlow (vOpen s))
    reachable i = [k | k <- vNext (vHere i s), Just k /= vPrev i s]

vInit :: Int -> Int -> Input -> VState
vInit minutes workers vMap = VState {..}
  where
    vPositions = replicate workers "AA"
    vPrevs = replicate workers Nothing
    vOpenK = mempty
    vMinute = 0
    vTotalTime = minutes
    vReleased = 0

better :: VState -> VState -> Bool
better a b = at <= bt && af >= bf && (at < bt || af > bf)
  where
    at = vMinute a
    bt = vMinute b
    af = vTotalFlow a
    bf = vTotalFlow b

-- | Whether given state (first argument) is an improvement on known states (second argument)
improves :: VState -> [VState] -> Maybe [VState]
improves a [] = Just [a]
improves a (b:bs)
  | a == b = Nothing
  | better b a = Nothing
  | better a b = improves a bs
  | otherwise = (b :) <$> improves a bs

type Seen = Map ([VKey], Set VKey) [VState]

initSeen :: Seen
initSeen = mempty

checkMarkSeen :: VState -> State Seen Bool
checkMarkSeen a = do
  let k = (vPositions a, vOpenK a)
  bs <- gets $ fromMaybe [] . Map.lookup k
  case improves a bs of
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
  vTotalFlow $
  head $
  traceF (unlines . map show) $
  minimumsOn vMinute $ maximumsOn vTotalFlow $ evalState (dfs st) initSeen
  where
    st = vInit minutes workers input

part1 = start 30 1

part2 = start 26 2

-- on my data the right answer to part 2 is between 2722 and 2922
tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651, Task part2 1707]
