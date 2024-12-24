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

type Wire = Text

data Op
  = OpAnd
  | OpOr
  | OpXor
  deriving (Ord, Eq, Show)

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

rDot :: Rules -> Text
rDot Rules {..} = dot fmt rGraph
  where
    fmt v = v <> maybe "" opSymbol (Map.lookup v rOps)

type Value = Map Wire Bit

mkRules :: [(Wire, Op, Set Wire)] -> Rules
mkRules rs = Rules {..}
  where
    rGraph = Map.fromList [(w, ws) | (w, _, ws) <- rs]
    rOps = Map.fromList [(w, op) | (w, op, _) <- rs]

parser :: Parser Text (Value, Rules)
parser =
  lineGroupsP
    &* (fmap Map.fromList (traverseP $ tsplitP ": " &* (idP &+ (charP &* bitP)))
          &+ fmap mkRules (traverseP ruleP))
  where
    ruleP = wordsP &* ap5P mkRule idP opP idP (requireP "->") idP
    opP = choiceP [("AND", OpAnd), ("OR", OpOr), ("XOR", OpXor)]
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
isOutputWire = Text.isPrefixOf "z"

isInputWire :: Wire -> Bool
isInputWire x = Text.head x `elem` ['x', 'y']

outputWires :: Rules -> [Wire]
outputWires = filter isOutputWire . toList . rWires

inputWires :: Rules -> [Wire]
inputWires r = do
  ow <- Text.tail <$> outputWires r
  p <- "xy"
  pure $ Text.cons p ow

prefixedValue :: Text -> Value -> Int
prefixedValue p =
  bitsValue . reverse . toList . Map.filterWithKey (\k v -> Text.isPrefixOf p k)

outputValue :: Value -> Int
outputValue = prefixedValue "z"

inputValues :: Value -> (Int, Int)
inputValues = (,) <$> prefixedValue "x" <*> prefixedValue "y"

part1 :: (Value, Rules) -> Int
part1 = uncurry evaluate . swap

tasks =
  Tasks
    2024
    24
    (CodeBlock 1)
    parser
    [ task part1 4 & taskScraper (CodeBlock 0)
    , task part1 2024 & taskPart 1
    ]
