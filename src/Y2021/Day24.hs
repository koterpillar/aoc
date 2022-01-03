{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2021.Day24 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import           AOC
import           Quantum         (Collapse (..), Quantum)
import qualified Quantum
import           Utils           hiding (Map)

data Register
  = W
  | X
  | Y
  | Z
  deriving (Ord, Eq, Show, Enum, Bounded)

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

iSource :: Instruction -> [Register]
iSource (Add r1 (SrcRegister r2)) = [r1, r2]
iSource (Add r1 (SrcValue _))     = [r1]
iSource (Mul r1 (SrcValue 0))     = []
iSource (Mul r1 (SrcRegister r2)) = [r1, r2]
iSource (Mul r1 (SrcValue _))     = [r1]
iSource (Div r1 (SrcRegister r2)) = [r1, r2]
iSource (Div r1 (SrcValue _))     = [r1]
iSource (Mod r1 (SrcRegister r2)) = [r1, r2]
iSource (Mod r1 (SrcValue _))     = [r1]
iSource (Eql r1 (SrcRegister r2)) = [r1, r2]
iSource (Eql r1 (SrcValue _))     = [r1]
iSource _                         = []

iDest :: Instruction -> Register
iDest (Inp r)   = r
iDest (Add r _) = r
iDest (Mul r _) = r
iDest (Div r _) = r
iDest (Mod r _) = r
iDest (Eql r _) = r

type Program = [Instruction]

data ALU =
  ALU
    { aluW :: !Int
    , aluX :: !Int
    , aluY :: !Int
    , aluZ :: !Int
    }
  deriving (Eq, Ord)

instance Show ALU where
  show (ALU w x y z) =
    "ALU W=" <> show w <> " X=" <> show x <> " Y=" <> show y <> " Z=" <> show z

aluInit :: ALU
aluInit = ALU 0 0 0 0

cheat = 26 ^ 5

aluSet :: Register -> Int -> ALU -> ALU
aluSet W w (ALU _ x y z) = ALU w x y z
aluSet X x (ALU w _ y z) = ALU w x y z
aluSet Y y (ALU w x _ z) = ALU w x y z
aluSet Z z (ALU w x y _) = ALU w x y (z `mod` cheat)

aluGet :: Src -> ALU -> Int
aluGet (SrcRegister W) (ALU w _ _ _) = w
aluGet (SrcRegister X) (ALU _ x _ _) = x
aluGet (SrcRegister Y) (ALU _ _ y _) = y
aluGet (SrcRegister Z) (ALU _ _ _ z) = z
aluGet (SrcValue v) _                = v

aluOp :: (Int -> Int -> Int) -> Register -> Src -> ALU -> ALU
aluOp f r1 r2 a = aluSet r1 (f (aluGet (SrcRegister r1) a) (aluGet r2 a)) a

newtype Max =
  Max [Int]
  deriving (Eq, Ord, Show)

newtype Min =
  Min [Int]
  deriving (Eq, Ord, Show)

instance Collapse Max where
  cinit = Max []
  cappend (Max a) (Max b) = Max $ b ++ a
  collapse (Max a) (Max b) = Max $ zipWith max a b

instance Collapse Min where
  cinit = Min []
  cappend (Min a) (Min b) = Min $ b ++ a
  collapse (Min a) (Min b) = Min $ zipWith min a b

class Collapse c =>
      DigitCollapse c
  where
  cdigit :: Int -> c
  cToList :: c -> [Int]

instance DigitCollapse Max where
  cdigit d = Max [d]
  cToList (Max a) = reverse a

instance DigitCollapse Min where
  cdigit d = Min [d]
  cToList (Min a) = reverse a

ichoose :: DigitCollapse c => Quantum c Int
ichoose = Quantum.fromList [(d, cdigit d) | d <- [1 .. 9]]

eql :: Int -> Int -> Int
eql x y
  | x == y = 1
  | otherwise = 0

runInstruction :: DigitCollapse c => Instruction -> ALU -> Quantum c ALU
runInstruction (Inp r) a     = Quantum.map (\d -> aluSet r d a) ichoose
runInstruction (Add r1 r2) a = Quantum.pure $ aluOp (+) r1 r2 a
runInstruction (Mul r1 r2) a = Quantum.pure $ aluOp (*) r1 r2 a
runInstruction (Div r1 r2) a = Quantum.pure $ aluOp div r1 r2 a
runInstruction (Mod r1 r2) a = Quantum.pure $ aluOp mod r1 r2 a
runInstruction (Eql r1 r2) a = Quantum.pure $ aluOp eql r1 r2 a

runProgram :: DigitCollapse c => Program -> Quantum c ALU -> Quantum c ALU
runProgram [] a = a
runProgram (i:is) a = a''
  where
    a' = traceF aluTrace $ Quantum.flatMap a $ runInstruction $ traceShowId i
    a'' = runProgram is a'

aluTrace :: DigitCollapse c => Quantum c ALU -> String
aluTrace as = "branches: " <> show (Quantum.size as)

success :: ALU -> Bool
success a = aluGet (SrcRegister Z) a == 0

inputsSingleKey :: (DigitCollapse c, Show c) => Quantum c a -> Maybe [Int]
inputsSingleKey i =
  case Quantum.toList $ Quantum.map (const ()) i of
    [(_, k)] -> Just $ cToList k
    []       -> Nothing
    err      -> error $ "inputsSingleKey: unexpected: " <> show err

dependsOn :: Instruction -> Instruction -> Bool
i1 `dependsOn` i2 = iDest i2 `elem` iSource i1

independent :: Instruction -> Instruction -> Bool
independent i1 i2 = not (i1 `dependsOn` i2) && not (i2 `dependsOn` i1)

weight :: Instruction -> Int
weight (Mul _ (SrcValue 0)) = -1
weight (Inp _)              = 1
weight _                    = 0

noop :: Instruction -> Bool
noop (Add _ (SrcValue 0)) = True
noop (Mul _ (SrcValue 1)) = True
noop (Div _ (SrcValue 1)) = True
noop _                    = False

optimize1 :: Program -> Program
optimize1 [] = []
optimize1 [i] = [i]
optimize1 (i1:i2:is)
  | independent i1 i2 && weight i1 > weight i2 = i2 : optimize1 (i1 : is)
  | otherwise = i1 : optimize1 (i2 : is)

optimize :: Program -> Program
optimize = iterateSettle optimize1 . filter (not . noop)

bestInput ::
     forall c. (DigitCollapse c, Show c)
  => Program
  -> Maybe [Int]
bestInput =
  inputsSingleKey @c .
  Quantum.filter success .
  flip runProgram (Quantum.pure aluInit) .
  listProgress 10 . traceShowF length . optimize . traceShowF length

part1 :: Program -> Maybe [Int]
part1 = bestInput @Max

part2 :: Program -> Maybe [Int]
part2 = bestInput @Min

traceShowIt :: (Show b) => (a -> b) -> a -> ()
traceShowIt f x = trace (show $ f x) ()

tasks =
  Tasks
    2021
    24
    (Inline "")
    parse
    [ Assert "part1" (Just [7]) $
      part1 [Inp Z, Add Z (SrcValue 2), Mod Z (SrcValue 3)]
    , Assert "part1 two inputs" (Just [7, 8]) $
      part1
        [ Inp Z
        , Add Z (SrcValue 2)
        , Mod Z (SrcValue 3)
        , Inp X
        , Add X (SrcValue 1)
        , Mod X (SrcValue 3)
        , Add Z (SrcRegister X)
        ]
    , Task (traceShowIt part1) ()
    , Task (traceShowIt part2) ()
    ]

parse :: Parser Text [Instruction]
parse = linesP &** instructionP

registerP :: Parser Text Register
registerP = charP &* choiceEBP "wxyz"

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
    instructionChoice "add" = pairPWith Add registerP srcP
    instructionChoice "mul" = pairPWith Mul registerP srcP
    instructionChoice "div" = pairPWith Div registerP srcP
    instructionChoice "mod" = pairPWith Mod registerP srcP
    instructionChoice "eql" = pairPWith Eql registerP srcP
    instructionChoice other =
      failP $ "Unknown instruction: " <> Text.unpack other
