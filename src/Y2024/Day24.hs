module Y2024.Day24
  ( tasks
  ) where

import           Data.Bits

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Bit
import           Graph
import           Memo
import           Utils

data Wire
  = X Int
  | Y Int
  | Z Int
  | M Text
  | XYXor Int Wire
  | XYAnd Int Wire
  | Carry Int Wire
  | Woo Int Wire
  deriving (Ord, Eq, Generic)

nshow :: Int -> String
nshow = Text.unpack . Text.justifyRight 2 '0' . tshow

instance Show Wire where
  show (X n)       = "x" ++ nshow n
  show (Y n)       = "y" ++ nshow n
  show (Z n)       = "z" ++ nshow n
  show (M t)       = Text.unpack t
  show (XYXor n w) = "XOR" ++ show n ++ show w
  show (XYAnd n w) = "AND" ++ show n ++ show w
  show (Carry n w) = "CAR" ++ show n ++ show w
  show (Woo n w)   = "WOO" ++ show n ++ show w

instance Hashable Wire

wireP :: Parser Text Wire
wireP = pureP Text.unpack &* unconsBindP go
  where
    intP' = pureP Text.pack &* integerP
    go 'x' = X <$> intP'
    go 'y' = Y <$> intP'
    go 'z' = Z <$> intP'
    go c   = M . Text.cons c <$> pureP Text.pack

data Op
  = OpAnd
  | OpOr
  | OpXor
  deriving (Ord, Eq, Show, Enum, Bounded)

opP :: Parser Text Op
opP = choiceEBP ["AND", "OR", "XOR"]

opEval :: Foldable f => Op -> f Bit -> Bit
opEval OpAnd = foldl bitAnd I
opEval OpOr  = foldl bitOr O
opEval OpXor = foldl bitXor O

opSymbol :: Op -> Text
opSymbol OpAnd = "A"
opSymbol OpOr  = "O"
opSymbol OpXor = "X"

data Rules = Rules
  { rGraph :: Graph Wire
  , rOps   :: Map Wire Op
  } deriving (Ord, Eq, Show)

rWires :: Rules -> Set Wire
rWires = Map.keysSet . rGraph

rBits :: Rules -> Int
rBits = length . outputWires

rOp :: Wire -> Rules -> Maybe Op
rOp w = Map.lookup w . rOps

rDot :: Rules -> Text
rDot r = dot fmt $ rGraph r
  where
    fmt v = tshow v <> maybe "" opSymbol (rOp v r)

type Swap = (Wire, Wire)

rSwap :: Swap -> Rules -> Rules
rSwap (a, b) r =
  r {rGraph = Map.mapKeys go $ rGraph r, rOps = Map.mapKeys go $ rOps r}
  where
    go w
      | w == a = b
      | w == b = a
      | otherwise = w

type Value = Map Wire Bit

mkRules :: [(Wire, Op, Set Wire)] -> Rules
mkRules rs = Rules {..}
  where
    rGraph = Map.fromList [(w, ws) | (w, _, ws) <- rs]
    rOps = Map.fromList [(w, op) | (w, op, _) <- rs]

parser :: Parser Text (Value, Rules)
parser =
  lineGroupsP
    &* (fmap
          Map.fromList
          (traverseP $ tsplitP ": " &* (wireP &+ (charP &* bitP)))
          &+ fmap mkRules (traverseP ruleP))
  where
    ruleP = wordsP &* ap5P mkRule wireP opP wireP (requireP "->") wireP
    mkRule a op b () r = (r, op, Set.fromList [a, b])

evaluate1 :: Map Wire Bit -> Rules -> Value
evaluate1 inputs Rules {..} =
  Map.fromList $ zip rk $ stateMemo go (`traverse` rk)
  where
    rk = Map.keys rGraph
    go rgo w =
      case Map.lookup w inputs of
        Just v -> pure v
        Nothing ->
          let deps = toList $ mapLookupE "evaluate1 deps" w rGraph
              op = mapLookupE "evaluate1 op" w rOps
           in opEval op <$> traverse rgo deps

evaluate :: Rules -> Value -> Int
evaluate rules v = outputValue $ evaluate1 v rules

isOutputWire :: Wire -> Bool
isOutputWire (Z _) = True
isOutputWire _     = False

isInputWire :: Wire -> Bool
isInputWire (X _) = True
isInputWire (Y _) = True
isInputWire _     = False

outputWires :: Rules -> [Wire]
outputWires = filter isOutputWire . toList . rWires

inputWires :: Rules -> [Wire]
inputWires r = do
  Z n <- outputWires r
  [X n, Y n]

outputValue :: Value -> Int
outputValue v = bitsValue $ reverse [v | (Z n, v) <- Map.toList v]

part1 :: (Value, Rules) -> Int
part1 = uncurry evaluate . swap

swapsToList :: Foldable f => f Swap -> [Wire]
swapsToList = sort . concatMap (\(a, b) -> [a, b])

assertNoForwardRefs :: Rules -> Rules
assertNoForwardRefs rules
  | any hasForwardRefs $ outputWires rules = error "assertNoForwardRefs"
  | otherwise = rules
  where
    hasForwardRefs w = any (isForwardRef w) $ reachableFrom1 w $ rGraph rules
    isForwardRef (Z z) (X x) = z < x
    isForwardRef (Z z) (Y y) = z < y
    isForwardRef _ _         = False

isExample :: Rules -> Bool
isExample = not . Map.member (Z 20) . rGraph

part2 :: (Value, Rules) -> Text
part2 (_, rules)
  | isExample rules = "(skip example)"
  | otherwise = part2' $ assertNoForwardRefs rules

allInputs :: Rules -> [Value]
allInputs rules =
  Map.fromList
    <$> traverse (\w -> (w, ) <$> [O, I]) (reverse $ inputWires rules)

findArg :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findArg f = listToMaybe . mapMaybe (\a -> (a, ) <$> f a)

doesNotCreateCycle :: Rules -> Swap -> Bool
doesNotCreateCycle rules (a, b) = not (reachable a b) && not (reachable b a)
  where
    reachable x y = Set.member y $ reachableFrom1 x $ rGraph rules

rMap :: (Graph Wire -> Graph Wire) -> Rules -> Rules
rMap f (Rules {..}) = Rules {rGraph = f rGraph, ..}

rMapWire :: (Wire -> Wire) -> Rules -> Rules
rMapWire f r =
  let r1 = rMap (mapGraph f) r
   in r1 {rOps = Map.mapKeys f $ rOps r1}

rDel :: Wire -> Rules -> Rules
rDel w Rules {..} =
  Rules {rGraph = Map.delete w rGraph, rOps = Map.delete w rOps}

rReplace :: Wire -> Wire -> Rules -> Rules
rReplace a b = rDel a . rDel b . rMapWire go
  where
    go w =
      if w == a
        then b
        else w

opportunity :: (a -> [a]) -> a -> a
opportunity f a = fromMaybe a $ listToMaybe $ f a

findXor :: Rules -> [Rules]
findXor r = do
  (a, as) <- Map.toList $ rGraph r
  guard $ rOp a r == Just OpXor
  [X x, Y y] <- [Set.toList as]
  guard $ x == y
  pure $ rReplace a (XYXor x a) r

findAnd :: Rules -> [Rules]
findAnd r = do
  (a, as) <- Map.toList $ rGraph r
  guard $ rOp a r == Just OpAnd
  [X x, Y y] <- [Set.toList as]
  guard $ x == y
  let b =
        if x == 0
          then Carry 1 a
          else XYAnd x a
  pure $ rReplace a b r

findWoo :: Rules -> [Rules]
findWoo r = do
  (a, as) <- Map.toList $ rGraph r
  guard $ rOp a r == Just OpAnd
  [XYXor x _, Carry y _] <- [Set.toList as]
  guard $ x == y
  pure $ rReplace a (Woo x a) r

findCarry :: Rules -> [Rules]
findCarry r = do
  (a, as) <- Map.toList $ rGraph r
  guard $ rOp a r == Just OpOr
  [XYAnd x _, Woo y _] <- [Set.toList as]
  guard $ x == y
  pure $ rReplace a (Carry (succ x) a) r

removeBoringZ :: Rules -> [Rules]
removeBoringZ r = do
  (Z z, as) <- Map.toList $ rGraph r
  guard $ rOp (Z z) r == Just OpXor
  [XYXor x _, Carry y _] <- [Set.toList as]
  guard $ x == z && y == z
  pure $ flip rMap r $ Map.insert (Z z) Set.empty

simplify :: Rules -> Rules
simplify =
  iterateSettleL
    $ opportunity removeBoringZ
        . opportunity findWoo
        . opportunity findCarry
        . opportunity findAnd
        . opportunity findXor

anonShow :: Rules -> Wire -> Text
anonShow r w = maybe "" opSymbol (rOp w r) <> go w
  where
    go (X _) = "X"
    go (Y _) = "Y"
    go (Z _) = "Z"
    go (M t) = "M"

characteristic :: Rules -> Int -> Wire -> Text
characteristic r 0 w = anonShow r w
characteristic r n w =
  Text.unwords
    $ (anonShow r w :)
    $ map (characteristic r $ pred n)
    $ toList
    $ neighbors w
    $ rGraph r

strange :: Rules -> [Wire]
strange rules =
  Set.toList
    $ Set.fromList
    $ ([M "krj", M "bpt"] ++)
    $ filter (not . exceptional)
    $ concatMap snd
    $ filter (\(_, ws) -> length ws < 5)
    $ Map.toList
    $ mapFromListS
        [(characteristic rules 2 w, [w]) | w <- toList $ rWires rules]
  where
    exceptional (X x) = x < 2
    exceptional (Y y) = y < 2
    exceptional (Z z) = z < 2
    exceptional _     = False

choose2 :: [a] -> [(a, a)]
choose2 xs = do
  a:xs' <- tails xs
  b <- xs'
  pure (a, b)

findSwap :: Rules -> (Swap, Rules)
findSwap rules =
  minimumOn
    (Text.length . rDot . simplify . snd)
    [(sw, rSwap sw rules) | sw <- choose2 $ strange rules]

part2' :: Rules -> Text
part2' rules =
  traceShow (strange rules)
    $ ttrace (rDot $ simplify r4)
    $ Text.intercalate ","
    $ sort
    $ concatMap (\(a, b) -> map tshow [a, b]) [s1, s2, s3, s4]
  where
    (s1, r1) = findSwap rules
    (s2, r2) = findSwap r1
    (s3, r3) = findSwap r2
    (s4, r4) = findSwap r3

tasks =
  Tasks
    (AOC 2024 24)
    (CodeBlock 1)
    parser
    [ task part1 4 & taskScraper (CodeBlock 0)
    , task part1 2024 & taskPart 1
    , taskBlind part2 & taskPart 2
    ]
