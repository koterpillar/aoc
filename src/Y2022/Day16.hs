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

instance Hashable VData where
  hashWithSalt s VData {..} = hashWithSalt s (vFlow, vNext)

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
    , vOpen      :: Set VKey
    , vRemaining :: Int
    , vReleased  :: Int
    }
  deriving (Eq, Ord, Show)

instance Hashable VState where
  hashWithSalt s VState {..} =
    hashWithSalt s (vMap, vPosition, vOpen, vRemaining, vReleased)

initState :: Input -> VState
initState vMap = VState {..}
  where
    vPosition = "AA"
    vOpen = mempty
    vRemaining = 30
    vReleased = 0

part1 input =
  vReleased $
  lastE "empty path" $
  fromJustE "no path" $ aStarDepth moves score ((== 0) . vRemaining) st
  where
    score s = maxScore - vReleased s
    maxScore = 100000000
    st = initState input
    moves s =
      map pssss $
      maybeToList (actOpen s) ++ map (actMove s) (reachable s) ++ [s]

pssss :: VState -> VState
pssss s = s {vRemaining = pred (vRemaining s), vReleased = vReleased s + flow}
  where
    flow =
      sum $ map vFlow $ mapMaybe (`Map.lookup` vMap s) $ Set.toList (vOpen s)

reachable :: VState -> [VKey]
reachable VState {..} = maybe [] vNext $ Map.lookup vPosition vMap

actOpen :: VState -> Maybe VState
actOpen s
  | vPosition s `Set.member` vOpen s = Nothing
  | otherwise = Just $ s {vOpen = Set.insert (vPosition s) (vOpen s)}

actMove :: VState -> VKey -> VState
actMove s k = s {vPosition = k}

tasks = Tasks 2022 16 (CodeBlock 0) parser [Task part1 1651]
