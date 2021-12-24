module Y2021.Day24 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import           AOC
import           Utils           hiding (Map)

data Register
  = W
  | X
  | Y
  | Z
  deriving (Ord, Eq, Show)

registers :: [Register]
registers = [W, X, Y, Z]

data Src
  = SrcRegister Register
  | SrcValue Int
  deriving (Ord, Eq)

instance Show Src where
  show (SrcRegister r) = show r
  show (SrcValue v)    = show v

data Instruction
  = Inp Register
  | Add Register Src
  | Mul Register Src
  | Div Register Src
  | Mod Register Src
  | Eql Register Src
  deriving (Ord, Eq, Show)

iSource :: Instruction -> Maybe Register
iSource (Add _ (SrcRegister r)) = Just r
iSource (Mul _ (SrcRegister r)) = Just r
iSource (Div _ (SrcRegister r)) = Just r
iSource (Mod _ (SrcRegister r)) = Just r
iSource (Eql _ (SrcRegister r)) = Just r
iSource _ = Nothing

iDest :: Instruction -> Register
iDest (Inp r) = r
iDest (Add r _) = r
iDest (Mul r _) = r
iDest (Div r _) = r
iDest (Mod r _) = r
iDest (Eql r _) = r

type Program = [Instruction]

newtype ALU =
  ALU
    { aluRegisters :: Map Register Int
    }
  deriving (Eq, Ord)

instance Show ALU where
  show (ALU rs) =
    "ALU " <> unwords (map (\(r, v) -> show r <> "=" <> show v) $ Map.toList rs)

aluInit :: ALU
aluInit = ALU $ Map.fromList $ zip registers $ repeat 0

cheat = 26 ^ 5

aluSet :: Register -> Int -> ALU -> ALU
aluSet Z v = ALU . Map.insert Z (v `mod` cheat) . aluRegisters
aluSet r v = ALU . Map.insert r v . aluRegisters

aluGet :: Src -> ALU -> Int
aluGet (SrcRegister r) =
  fromJustE ("aluGet: " <> show r) . Map.lookup r . aluRegisters
aluGet (SrcValue v) = const v

aluOp :: (Int -> Int -> Int) -> Register -> Src -> ALU -> ALU
aluOp f r1 r2 a = aluSet r1 (f (aluGet (SrcRegister r1) a) (aluGet r2 a)) a

newtype Inputs a =
  Inputs
    { getInputs :: Map a [Int]
    }
  deriving (Ord, Eq, Show)

iToList :: Inputs a -> [(a, [Int])]
iToList = Map.toList . getInputs

ipure :: Ord a => a -> Inputs a
ipure a = Inputs $ Map.singleton a []

ichoose :: Inputs Int
ichoose = Inputs $ Map.fromList [(d, [d]) | d <- [1 .. 9]]

imap :: Ord b => (a -> b) -> Inputs a -> Inputs b
imap f = Inputs . Map.mapKeysWith (zipWith max) f . getInputs

ijoin :: Ord a => Inputs (Inputs a) -> Inputs a
ijoin = Inputs . Map.fromListWith (zipWith max) . concatMap toListMul . iToList
  where
    toListMul (c, n) = [(v, m ++ n) | (v, m) <- iToList c]

iflatMap :: Ord b => Inputs a -> (a -> Inputs b) -> Inputs b
iflatMap a f = ijoin $ imap f a

ifilter :: (a -> Bool) -> Inputs a -> Inputs a
ifilter f = Inputs . Map.filterWithKey (\k _ -> f k) . getInputs

cheatMul :: Int -> Int -> Int
cheatMul a b = (a * b) `mod` 26

runInstruction :: Instruction -> ALU -> Inputs ALU
runInstruction (Inp r) a = flip imap ichoose $ \d -> aluSet r d a
runInstruction (Add r1 r2) a = ipure $ aluOp (+) r1 r2 a
runInstruction (Mul r1 r2) a = ipure $ aluOp (*) r1 r2 a
runInstruction (Div r1 r2) a = ipure $ aluOp div r1 r2 a
runInstruction (Mod r1 r2) a = ipure $ aluOp mod r1 r2 a
runInstruction (Eql r1 r2) a =
  ipure $
  aluOp
    (\x y ->
       if x == y
         then 1
         else 0)
    r1
    r2
    a

runProgram :: Program -> Inputs ALU -> Inputs ALU
runProgram [] a = a
runProgram (i:is) a = a''
  where
    a' = traceF aluTrace $ ijoin $ imap (runInstruction $ traceShowId i) a
    a'' = runProgram is a'

inputValues :: Inputs a -> [a]
inputValues = map fst . iToList

aluTrace :: Inputs ALU -> String
aluTrace as =
  "branches: " <> show size <> " " <> unwords (map showReg registers)
  where
    size = Map.size $ getInputs as
    showReg r = show r <> ": " <> showValues (aluGet $ SrcRegister r)
    showValues f = case inputValues $ imap f as of vs | length vs > 10 -> "(" <> show (length vs) <> ")"
                                                      | otherwise -> show vs

success :: ALU -> Bool
success a = aluGet (SrcRegister Z) a == 0

inputsSingleKey :: Inputs a -> Maybe [Int]
inputsSingleKey i =
  case Map.elems $ getInputs $ imap (const ()) i of
    [k] -> Just k
    []  -> Nothing
    err -> error $ "inputsSingleKey: unexpected: " <> show err

reorder1 :: Program -> Program
reorder1 [] = []
reorder1 ((Inp r1):i2:is) | iSource i2 /= Just r1 && iDest i2 /= r1 = i2 : reorder1 (Inp r1 : is)
reorder1 (i:is) = i:reorder1 is

reorder :: Program -> Program
reorder = iterateSettle reorder1

part1 :: Program -> Maybe [Int]
part1 =
  fmap reverse . inputsSingleKey .
  ifilter success . flip runProgram (ipure aluInit) . listProgress 18 . reorder . traceShowF length

part1' :: Program -> ()
part1' p = trace (show $ part1 p) ()

tasks =
  Tasks
    2021
    24
    parse
    [ Assert "part1" (Just [7]) $
      part1 [Inp Z, Add Z (SrcValue 2), Mod Z (SrcValue 3)]
    , Assert "part1 two inputs" (Just [7,8]) $
      part1 [Inp Z, Add Z (SrcValue 2), Mod Z (SrcValue 3), Inp X, Add X (SrcValue 1), Mod X (SrcValue 3), Add Z (SrcRegister X)]
    , Task part1' ()
    ]

parse :: Parser Text [Instruction]
parse = linesP &** instructionP

registerP :: Parser Text Register
registerP = charP &* choiceP (zip "wxyz" registers)

srcP :: Parser Text Src
srcP =
  Parser $ \t ->
    if isLower (Text.head t)
      then SrcRegister <$> runParse registerP t
      else SrcValue <$> runParse integerP t

instructionP :: Parser Text Instruction
instructionP = wordsP &* unconsP &* (idP &=> instructionChoice) &* pureP snd
  where
    instructionChoice "inp" = Inp <$> (singleP &* registerP)
    instructionChoice "add" = uncurry Add <$> (pairP &* (registerP &= srcP))
    instructionChoice "mul" = uncurry Mul <$> (pairP &* (registerP &= srcP))
    instructionChoice "div" = uncurry Div <$> (pairP &* (registerP &= srcP))
    instructionChoice "mod" = uncurry Mod <$> (pairP &* (registerP &= srcP))
    instructionChoice "eql" = uncurry Eql <$> (pairP &* (registerP &= srcP))
    instructionChoice other =
      failP $ "Unknown instruction: " <> Text.unpack other
