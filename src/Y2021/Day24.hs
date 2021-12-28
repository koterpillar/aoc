{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2021.Day24 where

import           Control.Monad.State

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils               hiding (Map)

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

type Program = [Instruction]

data BinOp
  = BAdd
  | BMul
  | BDiv
  | BMod
  | BEql
  deriving (Eq, Ord, Show)

binOp :: BinOp -> Int -> Int -> Int
binOp BAdd = (+)
binOp BMul = (*)
binOp BDiv = div
binOp BMod = mod
binOp BEql = eql

eql :: Int -> Int -> Int
eql x y
  | x == y = 1
  | otherwise = 0

data Expression
  = EConst Int
  | EInput Int
  | EB BinOp Expression Expression
  | ESwitch Expression [Expression] Expression Expression
  deriving (Eq, Ord)

instance Show Expression where
  showsPrec p (EConst v) = showsPrec p v
  showsPrec p (EInput v) = ("I" ++) . showsPrec p v
  showsPrec p (EB BAdd e1 e2) =
    showParen (p > 6) $ showsPrec 6 e1 . showString "+" . showsPrec 7 e2
  showsPrec p (EB BMul e1 e2) =
    showParen (p > 8) $ showsPrec 8 e1 . showString "*" . showsPrec 9 e2
  showsPrec p (EB BDiv e1 e2) =
    showParen (p > 8) $ showsPrec 8 e1 . showString "/" . showsPrec 9 e2
  showsPrec p (EB BMod e1 e2) =
    showParen (p > 8) $ showsPrec 8 e1 . showString "%" . showsPrec 9 e2
  showsPrec p (EB BEql e1 e2) =
    showParen (p >= 4) $ showsPrec 4 e1 . showString "==" . showsPrec 5 e2
  showsPrec p (ESwitch ec es et ef) =
    showParen (p >= 3) $
    showsPrec 3 ec .
    showString "in" .
    showsPrec 3 es .
    showString "?" . showsPrec 3 et . showString ":" . showsPrec 3 ef

newtype ALU =
  ALU
    { aluRegisters :: Map Register Expression
    }
  deriving (Eq, Ord)

initALU :: ALU
initALU = ALU Map.empty

instance Show ALU where
  show = unwords . map (uncurry showRE) . Map.toList . aluRegisters
    where
      showRE r e = show r ++ " = " ++ show e

mkInput :: State Int Expression
mkInput = do
  i <- get
  modify succ
  pure $ EInput i

applyInstruction2 :: BinOp -> Register -> Src -> ALU -> State Int ALU
applyInstruction2 op r s (ALU a) =
  pure $ ALU $ Map.insert r (EB op (aluGetR r) (aluGetS s)) a
  where
    aluGetR r = fromMaybe (EConst 0) $ Map.lookup r a
    aluGetS (SrcRegister r) = aluGetR r
    aluGetS (SrcValue v)    = EConst v

applyInstruction :: Instruction -> ALU -> State Int ALU
applyInstruction (Inp r) a =
  (\i -> ALU $ Map.insert r i $ aluRegisters a) <$> mkInput
applyInstruction (Add r v) a = applyInstruction2 BAdd r v a
applyInstruction (Mul r v) a = applyInstruction2 BMul r v a
applyInstruction (Div r v) a = applyInstruction2 BDiv r v a
applyInstruction (Mod r v) a = applyInstruction2 BMod r v a
applyInstruction (Eql r v) a = applyInstruction2 BEql r v a

synth' :: Program -> ALU
synth' p = evalState (foldM (flip applyInstruction) initALU p) 1

simplifyRec :: Expression -> Expression
simplifyRec (EConst v) = EConst v
simplifyRec (EInput n) = EInput n
simplifyRec (EB op e1 e2) =
  simplifySteps $ EB op (simplifyRec e1) (simplifyRec e2)
simplifyRec (ESwitch ec es et ef) =
  simplifySteps $
  ESwitch
    (simplifyRec ec)
    (map simplifyRec es)
    (simplifyRec et)
    (simplifyRec ef)

simplifySteps :: Expression -> Expression
simplifySteps =
  simplifySwitch .
  simplifyAssocConst .
  simplifyComm . simplifyDistr . simplifyIdempotent . simplifyConst

range :: Expression -> [Int]
range (EConst v) = [v]
range (EInput _) = [1 .. 9]
range (EB op e1 e2) = nubInt $ liftA2 (binOp op) (range e1) (range e2)
range (ESwitch ec es et ef) =
  nubInt $ ifL tpossible (range et) ++ ifL fpossible (range ef)
  where
    rc = Set.fromList $ range ec
    rs = Set.fromList $ concatMap range es
    exists = not . Set.null
    tpossible = exists $ Set.intersection rc rs
    fpossible = exists $ Set.difference rc rs
    ifL True xs = xs
    ifL False _ = []

simplifyConst :: Expression -> Expression
simplifyConst e =
  case range e of
    [v] -> EConst v
    _   -> e

simplifyIdempotent :: Expression -> Expression
simplifyIdempotent (EB BAdd e (EConst 0)) = e
simplifyIdempotent (EB BAdd (EConst 0) e) = e
simplifyIdempotent (EB BMul e (EConst 1)) = e
simplifyIdempotent (EB BMul (EConst 1) e) = e
simplifyIdempotent (EB BDiv e (EConst 1)) = e
simplifyIdempotent (EB BMod e (EConst 1)) = e
simplifyIdempotent e                      = e

simplifyDistr :: Expression -> Expression
simplifyDistr (EB BMul (EB BAdd e1 e2) e3) =
  EB BAdd (EB BMul e1 e3) (EB BMul e2 e3)
simplifyDistr (EB BDiv (EB BAdd e1 e2) e3) =
  EB BAdd (EB BDiv e1 e3) (EB BDiv e2 e3)
simplifyDistr e = e

canAssoc :: BinOp -> BinOp -> Maybe (BinOp, BinOp)
canAssoc BAdd BAdd = Just (BAdd, BAdd)
canAssoc BMul BMul = Just (BMul, BMul)
canAssoc BDiv BMul = Just (BMul, BDiv)
canAssoc _ _       = Nothing

simplifyAssocConst :: Expression -> Expression
simplifyAssocConst e@(EB op1 (EB op2 e1 e2@EConst {}) e3) =
  case canAssoc op1 op2 of
    Just (op1', op2') -> EB op1' e1 $ EB op2' e2 e3
    Nothing           -> e
simplifyAssocConst e = e

isConst :: Expression -> Bool
isConst (EConst _) = True
isConst _          = False

simplifyComm :: Expression -> Expression
simplifyComm = go BAdd . go BMul
  where
    go :: BinOp -> Expression -> Expression
    go op = scatter op . sortOn isConst . gather op
    gather :: BinOp -> Expression -> [Expression]
    gather op (EB op' e1 e2)
      | op == op' = gather op e1 ++ gather op e2
    gather _ e = [e]
    scatter :: BinOp -> [Expression] -> Expression
    scatter op = foldl1 (EB op)

simplifySwitch :: Expression -> Expression
simplifySwitch (EB BAdd (ESwitch ec es et ef) e2) =
  ESwitch ec es (EB BAdd et e2) (EB BAdd ef e2)
simplifySwitch (EB BMul e1 (ESwitch ec es et ef)) =
  ESwitch ec es (EB BMul e1 et) (EB BMul e1 ef)
simplifySwitch (EB BEql e1 e2) = ESwitch e1 [e2] (EConst 1) (EConst 0)
simplifySwitch (ESwitch (ESwitch ec es (EConst 1) (EConst 0)) [EConst 0] et ef) =
  ESwitch ec es et ef
simplifySwitch (ESwitch (EConst v) [e1] et ef) = ESwitch e1 [EConst v] et ef
simplifySwitch e = e

simplify :: Expression -> Expression
simplify = iterateSettle simplifyRec

simplifyALU :: ALU -> ALU
simplifyALU (ALU a) = ALU $ Map.map simplify a

synth :: Program -> ALU
synth = simplifyALU . synth'

part1 :: Program -> Maybe [Int]
part1 = undefined

part2 :: Program -> Maybe [Int]
part2 = undefined

traceShow_ :: (Show b) => (a -> b) -> a -> ()
traceShow_ f x = trace (show $ f x) ()

testSynth :: Program -> ()
testSynth p = traceShow (map go $ tail $ inits p) ()
  where
    go :: Program -> ()
    go p = trace (show $ synth $ traceShowF last p) ()

tasks =
  Tasks
    2021
    24
    parse
    [ Assert "synth" "X = I1*3+6" $
      show $ synth [Inp X, Add X (SrcValue 2), Mul X (SrcValue 3)]
    , Assert "synth eql 0" "X = 1" $ show $ synth [Eql X (SrcValue 0)]
    , Assert "simplify */" "I1*26" $
      show $ simplify $ EB BDiv (EB BMul (EInput 1) (EConst 676)) (EConst 26)
    , Assert "simplify (+)+(+)" "I1+I2+5" $
      show $
      simplify $
      EB BAdd (EB BAdd (EInput 1) (EConst 2)) (EB BAdd (EInput 2) (EConst 3))
    , Task testSynth ()
    -- , Task (traceShow_ synth) ()
    , Assert "part1" (Just [7]) $
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
    , Task (traceShow_ part1) ()
    , Task (traceShow_ part2) ()
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
