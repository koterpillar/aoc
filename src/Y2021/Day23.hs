module Y2021.Day23 where

import qualified Data.Map as Map

import           AOC
import           Grid
import           Path
import           Utils

data Amphi
  = A
  | B
  | C
  | D
  deriving (Ord, Eq, Show)

amphis :: [Amphi]
amphis = [A, B, C, D]

instance Hashable Amphi where
  hashWithSalt s a = hashWithSalt s (show a)

aEnergy :: Amphi -> Int
aEnergy A = 1
aEnergy B = 10
aEnergy C = 100
aEnergy D = 1000

data Situation =
  Situation
    { posHallway     :: !(Map Int Amphi)
    , posRooms       :: !(Map Amphi [Amphi])
    , posEnergySpent :: !Int
    }
  deriving (Ord, Eq, Show)

instance Hashable Situation where
  hashWithSalt s (Situation m1 m2 m3) = hashWithSalt s (m1, m2, m3)

hallwayX :: [Int]
hallwayX = [1 .. 11]

hallwayY :: Int
hallwayY = 1

hallwayBetween :: Int -> Int -> [Int]
hallwayBetween x1 x2
  | x1 > x2 = hallwayBetween x2 x1
  | otherwise = [x1 + 1 .. x2 - 1]

roomY :: [Int]
roomY = [2, 3]

roomX :: Amphi -> Int
roomX A = 3
roomX B = 5
roomX C = 7
roomX D = 9

posRoom :: Situation -> Amphi -> [Amphi]
posRoom s a = fromMaybe [] $ Map.lookup a $ posRooms s

posHallwayFree :: Situation -> Int -> Bool
posHallwayFree s x = Map.notMember x $ posHallway s

moves :: Situation -> [Situation]
moves s@Situation {..} =
  mapMaybe (moveFromHallway s) hallwayX ++ concatMap (moveFromRoom s) amphis

moveFromHallway :: Situation -> Int -> Maybe Situation
moveFromHallway s x = do
  a <- Map.lookup x $ posHallway s
  let targetX = roomX a
  let targetAs = posRoom s a
  guard $ all (== a) targetAs
  let targetY = last roomY - length targetAs
  traverse_ (guard . posHallwayFree s) (hallwayBetween x targetX)
  let energy = aEnergy a * abs (targetX - x) * abs (targetY - hallwayY)
  pure
    s
      { posHallway = Map.delete x $ posHallway s
      , posRooms = Map.adjust (a :) a $ posRooms s
      , posEnergySpent = posEnergySpent s + energy
      }

moveFromRoom :: Situation -> Amphi -> [Situation]
moveFromRoom s a' = do
  let x = roomX a'
  (a, y) <- maybeToList $ topInRoom s a'
  let targetXL =
        takeWhile (posHallwayFree s) $ dropWhile (>= x) $ reverse hallwayX
  let targetXR = takeWhile (posHallwayFree s) $ dropWhile (<= x) hallwayX
  targetX <- targetXL ++ targetXR
  let energy = aEnergy a * abs (targetX - x) * abs (y - hallwayY)
  pure
    s
      { posHallway = Map.insert targetX a $ posHallway s
      , posRooms = Map.adjust tail a' $ posRooms s
      , posEnergySpent = posEnergySpent s + energy
      }

-- situation -> room x -> (amphi, y)
topInRoom :: Situation -> Amphi -> Maybe (Amphi, Int)
topInRoom s a =
  case posRoom s a of
    []   -> Nothing
    a:as -> Just (a, head roomY - length as + 1)

isGoal :: Situation -> Bool
isGoal s@Situation {..} = all (\a -> posRoom s a == [a, a]) amphis

solve :: Situation -> Maybe [Situation]
solve =
  aStar
    (hashSetFromList . moves)
    (subtract `on` posEnergySpent)
    (const 0)
    isGoal

part1 :: Situation -> Int
part1 pos =
  posEnergySpent $ last $ fromJustE ("no solution for " <> show pos) $ solve pos

tasks :: Tasks
tasks =
  Tasks
    2021
    23
    parse
    [ AssertExample "top in room" (Just (B, 2)) (`topInRoom` A)
    , Task part1 12521
    ]

parse :: Parser Text Situation
parse =
  mkSituation . Map.mapMaybe id . fromMatrixG <$>
  linesP &** (charactersP &** parseChar)

mkSituation :: Map Position2 Amphi -> Situation
mkSituation grid = Situation {..}
  where
    posHallway = Map.empty
    posEnergySpent = 0
    posRooms =
      Map.fromList
        [ (a, catMaybes [Map.lookup (Position2 (roomX a) y) grid | y <- roomY])
        | a <- amphis
        ]

parseChar :: Parser Char (Maybe Amphi)
parseChar =
  choiceP $
  map (\a -> (head $ show a, Just a)) amphis ++ [(c, Nothing) | c <- ".# "]
