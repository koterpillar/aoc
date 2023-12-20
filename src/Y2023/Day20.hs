module Y2023.Day20 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Utils

type MKey = Text

data Pulse
  = High
  | Low
  deriving (Eq, Ord, Enum, Bounded)

instance Show Pulse where
  show High = "high"
  show Low  = "low"

data MType
  = FlipFlop
  | Conjunction
  | Input
  deriving (Eq, Ord, Show)

data Module = Module
  { mType         :: MType
  , mDestinations :: [MKey]
  } deriving (Eq, Ord, Show)

data Send = Send
  { sFrom  :: MKey
  , sPulse :: Pulse
  , sTo    :: MKey
  } deriving (Eq, Ord)

instance Show Send where
  show (Send f p t) = Text.unpack f <> " -" <> show p <> "-> " <> Text.unpack t

data Machine = Machine
  { maModules      :: Map MKey Module
  , maFlipFlops    :: Map MKey Bool
  , maConjunctions :: Map MKey (Map MKey Pulse)
  , maQueue        :: [Send]
  } deriving (Eq, Ord, Show)

input :: MKey
input = "broadcaster"

mkModule :: (Text, [Text]) -> (MKey, Module)
mkModule (n, ms)
  | n == input = (input, Module Input ms)
  | Text.isPrefixOf "%" n = (Text.drop 1 n, Module FlipFlop ms)
  | Text.isPrefixOf "&" n = (Text.drop 1 n, Module Conjunction ms)
  | otherwise = terror $ "Cannot parse module: " <> n <> " -> " <> tshow ms

mkMachine :: [(MKey, Module)] -> Machine
mkMachine ms = Machine {..}
  where
    maModules = Map.fromList ms
    inputsFor =
      mapFromListS [(dest, [src]) | (src, m) <- ms, dest <- mDestinations m]
    maFlipFlops = Map.fromList [(k, False) | (k, Module FlipFlop _) <- ms]
    maConjunctions =
      Map.fromList [(k, cInit k) | (k, Module Conjunction _) <- ms]
    cInit dest =
      Map.fromList [(src, Low) | src <- Map.findWithDefault [] dest inputsFor]
    maQueue = []

queue :: [Send] -> State Machine ()
queue sends = modify $ \ma -> ma {maQueue = maQueue ma ++ sends}

send :: Machine -> Maybe (Send, Machine)
send machine =
  case maQueue machine of
    [] -> Nothing
    (send@(Send k0 p k):q) ->
      Just
        $ (send, )
        $ flip execState machine
        $ do
            -- traceShowM send
            modify $ \ma -> ma {maQueue = tail $ maQueue ma}
            m' <- gets $ Map.lookup k . maModules
            for_ m' $ \m -> do
              ps' <-
                case mType m of
                  FlipFlop ->
                    if p == High
                      then pure Nothing
                      else do
                        f <- gets $ mapLookupE "send ff" k . maFlipFlops
                        let f' = not f
                        modify $ \ma ->
                          ma {maFlipFlops = Map.insert k f' $ maFlipFlops ma}
                        pure
                          $ Just
                          $ if f'
                              then High
                              else Low
                  Conjunction -> do
                    c <- gets $ mapLookupE "send c" k . maConjunctions
                    let c' = Map.insert k0 p c
                    modify $ \ma ->
                      ma {maConjunctions = Map.insert k c' $ maConjunctions ma}
                    pure
                      $ Just
                      $ if all (== High) c'
                          then Low
                          else High
                  Input -> pure $ Just p
              queue [Send k p' k' | p' <- toList ps', k' <- mDestinations m]

pushButton :: Machine -> ([Send], Machine)
pushButton ma
  | notNull $ maQueue ma =
    error $ "cannot push button while still settling: " <> show ma
  | otherwise = go $ ma {maQueue = [Send "button" Low input]}
  where
    go x =
      case send x of
        Nothing      -> ([], x)
        Just (s, x') -> first (s :) $ go x'

part1 :: Machine -> Int
part1 ma = countElem High r * countElem Low r
  where
    (r', _) = go 1000 ma
    r = map sPulse r'
    go :: Int -> Machine -> ([Send], Machine)
    go 0 m = ([], m)
    go n m = first (s ++) $ go (pred n) m'
      where
        (s, m') = pushButton m

allDestinations :: Machine -> Set MKey
allDestinations = Set.fromList . concatMap mDestinations . Map.elems . maModules

mst :: Machine -> Text
mst ma = Text.unlines [ffs, cos]
  where
    ffs = Text.unwords (map ff $ Map.toList $ maFlipFlops ma)
    ff (k, True)  = Text.toUpper k
    ff (k, False) = Text.toLower k
    cos = Text.unwords $ map co $ Map.toList $ maConjunctions ma
    co (c, cs) = c <> Text.pack (map p $ toList cs)
    p High = '#'
    p Low  = '.'

pushForever :: Machine -> [[Send]]
pushForever = unfoldr $ Just . pushButton

target2 :: MKey
target2 = "rx"

turnsOn :: Send -> Bool
turnsOn Send {..} = sPulse == Low && sTo == target2

sendsTo :: MKey -> Machine -> [MKey]
sendsTo t = filterTuple (elem t . mDestinations) . Map.toList . maModules

targetComponents :: Machine -> [MKey]
targetComponents m = sendsTo proxy m
  where
    proxy = fromSingleE "targetComponents.proxy" $ sendsTo target2 m

indices :: (a -> Bool) -> [a] -> [(Int, a)]
indices f = filter (f . snd) . zipN 0

indicesAgain :: (a -> Bool) -> [[a]] -> [(Int, Int, a)]
indicesAgain f xs = [(i, j, x) | (i, ys) <- zipN 0 xs, (j, x) <- indices f ys]

part2 :: Machine -> Int
part2 m
  | target2 `notElem` allDestinations m = 0
  | otherwise = error $ show $ foldr1 lSneakyMerge $ map lowsToTarget targets
  where
    allSends = pushForever m
    targets = traceShowF ("targets", ) $ targetComponents m
    lowsToTarget t =
      lFixSneaky
        $ lFrom
        $ map fst
        $ indices (any $ \s -> sTo s == t && sPulse s == Low) allSends

data Linear = Linear
  { lStart :: Int
  , lInc   :: Int
  } deriving (Show)

lFrom :: [Int] -> Linear
lFrom (a:b:c:_) =
  if c - b == b - a
    then Linear a (b - a)
    else error $ "lFrom: different deltas: " <> show [c - b, b - a]

lFixSneaky :: Linear -> Linear
lFixSneaky l@(Linear a b)
  | b == succ a = Linear (-1) b
  | otherwise = error $ "Expected origin at -1, got " <> show l

lSneakyMerge :: Linear -> Linear -> Linear
lSneakyMerge l1@(Linear a1 b1) l2@(Linear a2 b2)
  | a1 == negate 1 && a2 == negate 1 = Linear (-1) (lcm b1 b2)
  | otherwise =
    error $ "Expected origin at -1, got " <> show l1 <> " and " <> show l2

parser :: Parser Text Machine
parser =
  mkMachine . map mkModule
    <$> linesP &** (tsplitP " -> " &* (idP &+ tsplitP ", "))

tasks =
  Tasks
    2023
    20
    (CodeBlock 0)
    parser
    [ AssertExample "simple" 32000000 part1
    , task part1 11687500 & taskScraper (CodeBlock 2) & taskPart 1
    , taskBlind part2 & taskPart 2
    ]
