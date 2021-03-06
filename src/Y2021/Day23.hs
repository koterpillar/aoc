module Y2021.Day23 where

import qualified Data.Map as Map
import qualified Data.Set as Set

import           AOC
import           Grid
import           Path
import           Utils

data Amphi
  = A
  | B
  | C
  | D
  deriving (Ord, Eq, Bounded, Enum, Show)

amphis :: [Amphi]
amphis = boundedAll

instance Hashable Amphi where
  hashWithSalt s a = hashWithSalt s (show a)

aEnergy :: Amphi -> Int
aEnergy A = 1
aEnergy B = 10
aEnergy C = 100
aEnergy D = 1000

data Situation =
  Situation
    { posHallway   :: !(Map Int Amphi)
    , posRooms     :: !(Map Amphi [Amphi])
    , posRoomDepth :: !Int
    }
  deriving (Ord, Eq, Show)

instance Hashable Situation where
  hashWithSalt s (Situation h r d) = hashWithSalt s (h, r, d)

hallwayX :: [Int]
hallwayX = [1, 2, 4, 6, 8, 10, 11]

hallwayY :: Int
hallwayY = 1

hallwayBetween :: Int -> Int -> [Int]
hallwayBetween x1 x2
  | x1 > x2 = hallwayBetween x2 x1
  | otherwise = [x1 + 1 .. x2 - 1]

roomHeadY :: Int
roomHeadY = 2

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
moves s =
  mapMaybe (moveFromHallway s) hallwayX ++ concatMap (moveFromRoom s) amphis

moveFromHallway :: Situation -> Int -> Maybe Situation
moveFromHallway s x = do
  a <- Map.lookup x $ posHallway s
  let targetX = roomX a
  let targetAs = posRoom s a
  guard $ all (== a) targetAs
  let targetY = roomHeadY + posRoomDepth s - length targetAs - 1
  traverse_ (guard . posHallwayFree s) (hallwayBetween x targetX)
  let energy = aEnergy a * abs (targetX - x) * abs (targetY - hallwayY)
  pure
    s
      { posHallway = Map.delete x $ posHallway s
      , posRooms = Map.adjust (a :) a $ posRooms s
      }

moveFromRoom :: Situation -> Amphi -> [Situation]
moveFromRoom s a' = do
  guard $ not $ all (== a') $ posRoom s a'
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
      }

posRoomYs :: Situation -> Amphi -> [(Amphi, Int)]
posRoomYs s a = zip as [ys ..]
  where
    as = posRoom s a
    ys = roomHeadY + posRoomDepth s - length as

-- situation -> room x -> (amphi, y)
topInRoom :: Situation -> Amphi -> Maybe (Amphi, Int)
topInRoom s a = listToMaybe $ posRoomYs s a

isGoal :: Situation -> Bool
isGoal s@Situation {..} =
  all (\a -> posRoom s a == replicate posRoomDepth a) amphis

targetEstimate :: Situation -> Int
targetEstimate s@Situation {..} = sum estimateHallway + sum estimateRooms
  where
    estimateHallway = map (uncurry estimateHallwayA) $ Map.toList posHallway
    estimateHallwayA x a = moveEnergy a (Position2 x hallwayY) (target a)
    estimateRooms = concatMap estimateRoom amphis
    estimateRoom a' =
      if posRoom s a' == replicate posRoomDepth a'
        then []
        else map (uncurry $ estimateRoomA (roomX a')) (posRoomYs s a')
    -- FIXME overestimates
    estimateRoomA x a y =
      sum $ map (moveEnergy a (Position2 x hallwayY)) [Position2 x y] {-, target a-}
    target a = Position2 (roomX a) roomHeadY

moveEnergy :: Amphi -> Position2 -> Position2 -> Int
moveEnergy a (Position2 x1 y1) (Position2 x2 y2) =
  aEnergy a * (abs (x2 - x1) + abs (y2 - y1))

apositions :: Situation -> Set (Amphi, Position2)
apositions s = Set.fromList (hallway ++ rooms)
  where
    hallway = [(a, Position2 x hallwayY) | (x, a) <- Map.toList $ posHallway s]
    rooms = do
      a' <- amphis
      (a, y) <- posRoomYs s a'
      pure (a, Position2 (roomX a') y)

energySpent :: Situation -> Situation -> Int
energySpent s1 s2 =
  if a1 /= a2
    then error
           ("Inconsistent positions: " <>
            show (a1, p1) <> " /= " <> show (a2, p2))
    else moveEnergy a1 p1 p2
  where
    ps1 = apositions s1
    ps2 = apositions s2
    (a1, p1) = fromSingleE (show (s1, s2)) $ Set.toList $ Set.difference ps1 ps2
    (a2, p2) = fromSingleE (show (s1, s2)) $ Set.toList $ Set.difference ps2 ps1

fromSingleE :: Show a => String -> [a] -> a
fromSingleE _ [a]  = a
fromSingleE msg as = error $ "fromSingleE: " <> show as <> ": " <> msg

solve :: Situation -> Maybe [Situation]
solve = aStar (hashSetFromList . moves) energySpent targetEstimate isGoal

totalEnergySpent :: [Situation] -> Int
totalEnergySpent = sum . zipWithTail energySpent

toPart2 :: Situation -> Situation
toPart2 s = s {posRoomDepth = posRoomDepth s + 2, posRooms = rooms'}
  where
    rooms' =
      posRooms s & Map.adjust (insert1 [D, D]) A & Map.adjust (insert1 [C, B]) B &
      Map.adjust (insert1 [B, A]) C &
      Map.adjust (insert1 [A, C]) D
    insert1 :: [a] -> [a] -> [a]
    insert1 [a2, a3] (a1:as) = a1 : a2 : a3 : as
    insert1 _ _              = error "insert1"

part1 :: Situation -> Int
part1 pos =
  totalEnergySpent $
  map traceShowId $
  (pos :) $ fromJustE ("no solution for " <> show pos) $ solve $ traceShowId pos

part2 :: Situation -> Int
part2 = part1 . toPart2

tasks :: Tasks
tasks =
  Tasks
    2021
    23
    (CodeBlock 0)
    parse
    [ AssertExample "top in room" (Just (B, 2)) (`topInRoom` A)
    , Task part1 12521
    , Task part2 44169
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
    posRoomDepth = 2
    posRooms =
      Map.fromList
        [ ( a
          , catMaybes
              [ Map.lookup (Position2 (roomX a) y) grid
              | y <- [roomHeadY .. roomHeadY + posRoomDepth]
              ])
        | a <- amphis
        ]

parseChar :: Parser Char (Maybe Amphi)
parseChar =
  choiceP $
  map (\a -> (head $ show a, Just a)) amphis ++ [(c, Nothing) | c <- ".# "]
