module Y2023.Day20 where

import           Control.Monad.State.Strict

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           AOC
import           Graph                      (dot)
import           Utils

type MKey = Text

data Pulse
  = High
  | Low
  deriving (Eq, Ord, Enum, Bounded)

instance Show Pulse where
  show High = "hi"
  show Low  = "lo"

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
  { maModules      :: !(Map MKey Module)
  , maFlipFlops    :: !(Map MKey Bool)
  , maConjunctions :: !(Map MKey (Map MKey Pulse))
  , maQueue        :: ![Send]
  , maHistory      :: ![Send]
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
    maHistory = []

maSend :: [Send] -> State Machine ()
maSend sends =
  modify $ \ma ->
    ma
      {maQueue = maQueue ma ++ sends, maHistory = reverse sends ++ maHistory ma}

maDequeue :: State Machine (Maybe Send)
maDequeue = gets maQueue >>= go
  where
    go :: [Send] -> State Machine (Maybe Send)
    go [] = pure Nothing
    go (s:ss) = do
      modify $ \ma -> ma {maQueue = ss}
      pure $ Just s

maCycle :: State Machine Bool
maCycle = maDequeue >>= go
  where
    go Nothing = pure False
    go (Just (Send k0 p k)) = do
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
        maSend [Send k p' k' | p' <- toList ps', k' <- mDestinations m]
      pure True

maPushButton :: State Machine ()
maPushButton = do
  initialQueue <- gets maQueue
  when (notNull initialQueue) $ error "cannot push button while still settling"
  maSend [Send "button" Low input]
  whileM maCycle

part1 :: Machine -> Int
part1 ma = countElem High r * countElem Low r
  where
    r =
      map sPulse
        $ maHistory
        $ execState (replicateM_ 1000 maPushButton) ma

allDestinations :: Machine -> Set MKey
allDestinations = Set.fromList . concatMap mDestinations . Map.elems . maModules

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

part2 :: Machine -> Int
part2 m
  | target2 `notElem` allDestinations m = 0
  | otherwise = error "part2a"

modifyTarget :: (MKey -> MKey -> Bool) -> Machine -> Machine
modifyTarget f ma =
  ma
    { maModules =
        Map.mapWithKey
          (\k m -> m {mDestinations = filter (f k) $ mDestinations m})
          $ maModules ma
    }

maDot :: Machine -> Text
maDot ma = dot id $ Map.map (Set.fromList . mDestinations) $ maModules ma

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
