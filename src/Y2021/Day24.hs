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

data Expression
  = EConst Int
  | EInput Int
  | EAdd Expression Expression
  | EMul Expression Expression
  | EDiv Expression Expression
  | EMod Expression Expression
  | EEql Expression Expression
  | ESwitch Expression [Expression] Expression Expression
  deriving (Eq, Ord)

instance Show Expression where
  showsPrec p (EConst v) = showsPrec p v
  showsPrec p (EInput v) = ("I" ++) . showsPrec p v
  showsPrec p (EAdd e1 e2) =
    showParen (p > 6) $ showsPrec 6 e1 . showString "+" . showsPrec 6 e2
  showsPrec p (EMul e1 e2) =
    showParen (p > 7) $ showsPrec 7 e1 . showString "*" . showsPrec 7 e2
  showsPrec p (EDiv e1 e2) =
    showParen (p > 7) $ showsPrec 7 e1 . showString "/" . showsPrec 7 e2
  showsPrec p (EMod e1 e2) =
    showParen (p > 7) $ showsPrec 7 e1 . showString "%" . showsPrec 7 e2
  showsPrec p (EEql e1 e2) =
    showParen (p >= 4) $ showsPrec 4 e1 . showString "==" . showsPrec 4 e2
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

applyInstruction2 ::
     (Expression -> Expression -> Expression)
  -> Register
  -> Src
  -> ALU
  -> State Int ALU
applyInstruction2 mkE r s (ALU a) =
  pure $ ALU $ Map.insert r (mkE (aluGetR r) (aluGetS s)) a
  where
    aluGetR r = fromMaybe (EConst 0) $ Map.lookup r a
    aluGetS (SrcRegister r) = aluGetR r
    aluGetS (SrcValue v)    = EConst v

applyInstruction :: Instruction -> ALU -> State Int ALU
applyInstruction (Inp r) a =
  (\i -> ALU $ Map.insert r i $ aluRegisters a) <$> mkInput
applyInstruction (Add r v) a = applyInstruction2 EAdd r v a
applyInstruction (Mul r v) a = applyInstruction2 EMul r v a
applyInstruction (Div r v) a = applyInstruction2 EDiv r v a
applyInstruction (Mod r v) a = applyInstruction2 EMod r v a
applyInstruction (Eql r v) a = applyInstruction2 EEql r v a

synth' :: Program -> ALU
synth' p = evalState (foldM (flip applyInstruction) initALU p) 1

simplify :: Expression -> Expression
simplify (EConst v) = EConst v
simplify (EInput n) = EInput n
simplify (EAdd e1 e2) = simplifySteps $ EAdd (simplify e1) (simplify e2)
simplify (EMul e1 e2) = simplifySteps $ EMul (simplify e1) (simplify e2)
simplify (EDiv e1 e2) = simplifySteps $ EDiv (simplify e1) (simplify e2)
simplify (EMod e1 e2) = simplifySteps $ EMod (simplify e1) (simplify e2)
simplify (EEql e1 e2) = simplifySteps $ EEql (simplify e1) (simplify e2)
simplify (ESwitch ec es et ef) =
  simplifySteps $
  ESwitch (simplify ec) (map simplify es) (simplify et) (simplify ef)

simplifySteps = simplify' . simplifyIdempotent . simplifyConst

eql :: Int -> Int -> Int
eql x y
  | x == y = 1
  | otherwise = 0

range :: Expression -> [Int]
range (EConst v) = [v]
range (EInput _) = [1 .. 9]
range (EAdd e1 e2) = nubInt $ liftA2 (+) (range e1) (range e2)
range (EMul e1 e2) = nubInt $ liftA2 (*) (range e1) (range e2)
range (EDiv e1 e2) = nubInt $ liftA2 div (range e1) (range e2)
range (EMod e1 e2) = nubInt $ liftA2 mod (range e1) (range e2)
range (EEql e1 e2) = nubInt $ liftA2 eql (range e1) (range e2)
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
simplifyIdempotent (EAdd e (EConst 0)) = e
simplifyIdempotent (EAdd (EConst 0) e) = e
simplifyIdempotent (EMul e (EConst 1)) = e
simplifyIdempotent (EMul (EConst 1) e) = e
simplifyIdempotent (EDiv e (EConst 1)) = e
simplifyIdempotent (EMod e (EConst 1)) = e
simplifyIdempotent e                   = e

simplify' :: Expression -> Expression
simplify' (EAdd (ESwitch ec es et ef) e2) =
  ESwitch ec es (EAdd et e2) (EAdd ef e2)
simplify' (EMul e1 (ESwitch ec es et ef)) =
  ESwitch ec es (EMul e1 et) (EMul e1 ef)
simplify' (EEql e1 e2) = ESwitch e1 [e2] (EConst 1) (EConst 0)
simplify' (ESwitch (ESwitch ec es (EConst 1) (EConst 0)) [EConst 0] et ef) =
  ESwitch ec es et ef
simplify' (ESwitch (EConst v) [e1] et ef) = ESwitch e1 [EConst v] et ef
simplify' e = e

simplifyALU :: ALU -> ALU
simplifyALU (ALU a) = ALU $ Map.map (iterateSettle simplify) a

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
    [ Assert "synth" "X = (I1+2)*3" $
      show $ synth [Inp X, Add X (SrcValue 2), Mul X (SrcValue 3)]
    , Assert "synth eql 0" "X = 1" $ show $ synth [Eql X (SrcValue 0)]
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
