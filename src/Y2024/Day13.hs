module Y2024.Day13
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Grid
import           Path
import           Utils

data Button p = Button
  { bStep  :: p
  , bCost  :: Int
  , bScale :: Int
  } deriving (Ord, Eq, Show, Functor, Foldable, Traversable)

bMultiply :: Int -> Button Position2 -> Button Position2
bMultiply n b
  | n <= 0 = error $ "bMultiply: " <> show n
  | otherwise =
    b {bStep = pointM n $ bStep b, bCost = n * bCost b, bScale = n * bScale b}

data Machine' p = Machine
  { mButtons   :: Map Char (Button p)
  , mPrize     :: p
  , mExtraCost :: Int
  } deriving (Ord, Eq, Show, Functor, Foldable, Traversable)

type Machine = Machine' Position2

mButton :: Char -> Machine' p -> Button p
mButton b = mapLookupE "mButton" b . mButtons

mOtherButton :: Char -> Machine -> Button Position2
mOtherButton b m =
  case filter ((/= b) . fst) (Map.toList $ mButtons m) of
    [(_, r)] -> r
    []       -> error "mOtherButton: no other button"
    _        -> error "mOtherButton: multiple other buttons"

mMultiply :: Char -> Int -> Machine -> Machine
mMultiply b n m = m {mButtons = Map.adjust (bMultiply n) b $ mButtons m}

mPress :: Char -> Int -> Machine -> Machine
mPress b n m =
  m
    { mPrize = mPrize m `pointMinus` (n `pointM` bStep button)
    , mExtraCost = mExtraCost m + n * bCost button
    }
  where
    button = mButton b m

mWon :: Machine -> Bool
mWon m = mPrize m == Position2 0 0

machineLineP :: Parser Text Position2
machineLineP = pureP cleanup &* wordsP &* ap2P Position2 integerP integerP
  where
    cleanup =
      Text.map
        (\c ->
           if not $ isDigit c
             then ' '
             else c)

mkMachine :: Position2 -> Position2 -> Position2 -> Machine
mkMachine a b p = Machine buttons p 0
  where
    buttons = Map.fromList [('A', Button a 3 1), ('B', Button b 1 1)]

parser :: Parser Text [Machine]
parser = lineGroupsP &** ap3P mkMachine machineLineP machineLineP machineLineP

type Presses = Map Char Int

underLimit :: Maybe Int -> Int -> Bool
underLimit Nothing      = const True
underLimit (Just limit) = (limit >=)

nextMoves :: Machine -> [Int] -> Maybe Int -> Presses -> [Presses]
nextMoves m scale limit pp = do
  (b, button) <- Map.toList $ mButtons m
  let p = fromMaybe 0 $ Map.lookup b pp
  v <- scale
  let p' = p + v
  guard $ underLimit limit $ p' * bScale button
  let pp' = Map.insert b p' pp
  guard $ winnable m $ target m pp'
  pure pp'

pressesCost :: Machine -> Presses -> Int
pressesCost m pp = sum [n * bCost (mButton b m) | (b, n) <- Map.toList pp]

moveCost :: Machine -> Presses -> Presses -> Int
moveCost m = (-) `on` pressesCost m

target :: Machine -> Presses -> Position2
target m = foldMap (uncurry go) . Map.toList
  where
    go b n = pointM n $ bStep $ mButton b m

simplify :: Machine -> Machine
simplify =
  traceShowF ("s-result", )
    . iterateSettleL simplify1
    . traceShowF ("original", )

coordsList :: [(Char, Position2 -> Int)]
coordsList = [('x', pX), ('y', pY)]

forCoords :: (Char -> (Position2 -> Int) -> a -> a) -> a -> a
forCoords f = flip (foldr $ uncurry f) coordsList

simplify1 :: Machine -> Machine
simplify1 = simplifyOffset . simplifyOdd . simplifyGcd

gcd_ :: (Functor f, Foldable f) => (p -> Int) -> f p -> Int
gcd_ coord = foldr1 gcd . fmap coord

mGcd :: Machine -> Position2
mGcd m = Position2 (go pX) (go pY)
  where
    go = flip gcd_ m

simplifyGcd :: Machine -> Machine
simplifyGcd m =
  forCoords
    (\coordChar coord ->
       if coord r == 1
         then id
         else trace ("Dividing " <> [coordChar] <> " by GCD " <> show (coord r)))
    $ fmap (posDiv $ mGcd m) m
  where
    r = mGcd m
    posDiv (Position2 dx dy) (Position2 x y) =
      Position2 (x `div` dx) (y `div` dy)

forButtons ::
     (Char -> Button p -> Machine' p -> Machine' p) -> Machine' p -> Machine' p
forButtons go m0 =
  foldr (\b m -> go b (mButton b m) m) m0 $ Map.keys $ mButtons m0

simplifyOdd :: Machine -> Machine
simplifyOdd = forCoords goCoord
  where
    goCoord coordChar coord = forButtons go
      where
        go :: Char -> Button Position2 -> Machine -> Machine
        go c b m
          | mWon m = m
          | gRest /= 1 =
            trace
              ("Button "
                 <> [c]
                 <> " has "
                 <> [coordChar]
                 <> " coordinate "
                 <> show bY
                 <> " and without it GCD is "
                 <> show gRest)
              $ simplifyGcd
              $ mMultiply c gRest m
          | otherwise = m
          where
            mRest = m {mButtons = Map.delete c $ mButtons m}
            bY = coord $ bStep b
            gRest = coord $ mGcd mRest

mImpossible :: Machine
mImpossible =
  Machine
    { mButtons = Map.fromList [('A', Button step 1 1), ('B', Button step 1 1)]
    , mPrize = prize
    , mExtraCost = 0
    }
  where
    step = Position2 10 10
    prize = Position2 1 1

simplifyOffset :: Machine -> Machine
simplifyOffset = forCoords goC
  where
    goC coordChar coord = forButtons go
      where
        go buttonChar button1 m
          | cp == 0 = m
          | gcd c1 c2 > 1 && gcd c1 cp == 1 && gcd c2 cp == 1 =
            trace
              ("IMPOSSIBLE MACHINE: buttons GCD "
                 <> show (gcd c1 c2)
                 <> " but 1 with prize")
              mImpossible
          | gcd c1 c2 == 1 && gcd c1 cp == 1 =
            case findPresses c1 c2 cp of
              Nothing -> traceShow ("IMPOSSIBLE MACHINE", m) mImpossible
              Just presses
                | presses > 0 ->
                  traceShowF ("offset >", )
                    $ trace
                        ("buttons "
                           <> [coordChar]
                           <> ": "
                           <> show c1
                           <> " "
                           <> show c2
                           <> "; prize "
                           <> show cp
                           <> "; pressing "
                           <> show presses
                           <> " times")
                    $ mPress buttonChar presses m
                | otherwise -> m
          | otherwise = m
          where
            presses = findPresses c1 c2 cp
            button2 = mOtherButton buttonChar m
            bc = coord . bStep
            c1 = coord $ bStep button1
            c2 = coord $ bStep button2
            cp = coord $ mPrize m

findPresses :: Int -> Int -> Int -> Maybe Int
findPresses c1 c2 cp
  | cp < 0 = Nothing
  | c2 == 1 = Just 0
  | gcd c2 cp > 1 = Just 0
  | otherwise = fmap succ $ findPresses c1 c2 $ cp - c1

winning :: Maybe Int -> Machine -> Maybe Int
winning l = winning' l . simplify

totalCost :: Machine -> Presses -> Int
totalCost m pp = mExtraCost m + pressesCost m pp

winning' :: Maybe Int -> Machine -> Maybe Int
winning' limit m = traceShowF (m, r, ) $ fmap (totalCost m) r
  where
    steps = take 12 $ iterate (* 10) 1
    r =
      fromMaybe mempty . listToMaybe . reverse
        <$> aStar
              (nextMoves m steps limit)
              (moveCost m)
              distanceToGoal
              isGoal
              Map.empty
    isGoal p = target m p == mPrize m
    distanceToGoal p = manhattanDistance t prize
      where
        t = target m p
        prize = mPrize m

estimateToGoal :: Machine -> Presses -> Int
estimateToGoal m p =
  traceShowF ("distance estimate", m, p, )
    $ minimum
    $ do
        (b, button) <- Map.toList $ mButtons m
        pCoord <- [pX, pY]
        let amount =
              (pCoord prize - pCoord (target m p)) `div` pCoord (bStep button)
        pure $ pressesCost m $ Map.singleton b amount
  where
    prize = mPrize m

winnable :: Machine -> Position2 -> Bool
winnable m t
  | pX d < 0 = False
  | pY d < 0 = False
  | overshot pX pY = False
  | overshot pY pX = False
  | otherwise = True
  where
    d = mPrize m `pointMinus` t
    overshot :: (Position2 -> Int) -> (Position2 -> Int) -> Bool
    overshot pTry pResult =
      all (overshotButton pTry pResult) $ Map.elems $ mButtons m
    overshotButton ::
         (Position2 -> Int) -> (Position2 -> Int) -> Button Position2 -> Bool
    overshotButton pTry pResult mButton = maxResult < prizeResult
      where
        prizeResult = pResult (mPrize m)
        pButton = bStep mButton
        maxResult = pResult t + pResult pButton * maxSteps
        maxSteps = (pTry (mPrize m) - pTry t) `div` pTry pButton

part1 :: [Machine] -> Int
part1 = sum . mapMaybe (winning $ Just 100)

harderPrize :: Machine -> Machine
harderPrize m = m {mPrize = mPrize m `pointPlus` Position2 offset offset}
  where
    offset = 10000000000000

part2 :: [Machine] -> Int
part2 = sum . mapMaybe (winning Nothing . harderPrize)

testMachine :: Machine
testMachine = mkMachine (Position2 10 1) (Position2 1 10) (Position2 p p)
  where
    p = 11

tasks =
  Tasks
    (AOC 2024 13)
    (CodeBlock 0)
    parser
    [ task part1 480 & taskPart 1
    , Assert "testMachine win" (Just 4) $ winning Nothing testMachine
    , taskBlind part2 & taskPart 2
    ]
