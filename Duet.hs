{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Duet where

import Control.Lens

import Data.Either

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Utils

type Register = Char

type Value = Int

data Expr
  = SRegister Register
  | SValue Value
  deriving (Eq, Ord, Show)

type Offset = Int

data Instruction
  = Snd Expr
  | Set Expr
        Expr
  | Add Expr
        Expr
  | Mul Expr
        Expr
  | Mod Expr
        Expr
  | Rcv Expr
  | Jgz Expr
        Expr
  deriving (Ord, Eq, Show)

type Registers = M.Map Register Value

type Program = [Instruction]

type Address = Int

data Computer = Computer
  { _cRegisters :: Registers
  , _cProgram :: Program
  , _cIP :: Address
  } deriving (Eq, Ord, Show)

makeLenses ''Computer

-- a computer awaiting a value that will be put into given register
data Paused =
  Paused Register
         Computer
  deriving (Eq, Ord, Show)

data Duet = Duet
  { _dComp0 :: Either Paused Computer
  , _dComp1 :: Either Paused Computer
  , _dSentBy0 :: [Value]
  , _dSentBy1 :: [Value]
  , _dTotalSent1 :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''Duet

resume :: Value -> Paused -> Computer
resume v (Paused r c) = cAdvance $ (cRegister r .~ v) c

bootDuet :: Program -> Duet
bootDuet p =
  Duet
  { _dComp0 = Right $ boot 0 p
  , _dComp1 = Right $ boot 1 p
  , _dSentBy0 = []
  , _dSentBy1 = []
  , _dTotalSent1 = 0
  }

registers :: [Register]
registers = ['a' .. 'z']

boot :: Value -> Program -> Computer
boot programID instructions =
  Computer
    (M.insert 'p' programID $ M.fromList $ zip registers (repeat 0))
    instructions
    0

stopped :: Computer -> Bool
stopped c =
  let ip = c ^. cIP
  in ip < 0 || ip >= length (c ^. cProgram)

unsafeMaybeLens :: Iso' (Maybe a) a
unsafeMaybeLens = iso fromJust Just

cRegister :: Register -> Lens' Computer Value
cRegister r = cRegisters . at r . unsafeMaybeLens

cInstruction :: Getter Computer Instruction
cInstruction = to $ \c -> fromJust $ c ^? cProgram . ix (c ^. cIP)

cAdvance :: Computer -> Computer
cAdvance = cIP +~ 1

eval :: Computer -> Expr -> Value
eval comp expr =
  case expr of
    (SValue val) -> val
    (SRegister reg) -> comp ^. cRegister reg

type PushPull = Either Paused (Maybe Value, Computer)

step :: Computer -> PushPull
step c = go (c ^. cInstruction) c
  where
    go (Snd x) = emit (eval c x) . cAdvance
    go (Set (SRegister x) y) = norm . cAdvance . (cRegister x .~ eval c y)
    go (Add (SRegister x) y) = norm . cAdvance . (cRegister x +~ eval c y)
    go (Mul (SRegister x) y) = norm . cAdvance . (cRegister x *~ eval c y)
    go (Mod (SRegister x) y) =
      norm . cAdvance . (cRegister x %~ (`mod` eval c y))
    go (Rcv (SRegister x)) = Left . Paused x
    go (Jgz x y) =
      if eval c x > 0
        then norm . (cIP +~ eval c y)
        else norm . cAdvance
    emit :: Value -> Computer -> PushPull
    emit v c = Right (Just v, c)
    norm :: Computer -> PushPull
    norm c = Right (Nothing, c)

duetStopped :: Duet -> Bool
duetStopped d =
  stoppedPair (d ^. dComp0) (d ^. dSentBy1) &&
  stoppedPair (d ^. dComp1) (d ^. dSentBy0)
  where
    stoppedPair (Right c) _ = stopped c
    stoppedPair (Left _) [] = True
    stoppedPair (Left _) _ = False

stepDuet :: Duet -> Duet
stepDuet d =
  case d ^. dComp1 of
    Left p1 ->
      case d ^. dSentBy0 of
        [] ->
          case d ^. dComp0 of
            Left p0 ->
              case d ^. dSentBy1 of
                [] -> error "deadlock in step!"
                (v:vs) -> d & dComp0 .~ Right (resume v p0) & dSentBy1 .~ vs
            Right c0 ->
              case step c0 of
                Right (Just v, c0') ->
                  d & dSentBy0 %~ (`snoc` v) & dComp0 .~ Right c0'
                Right (Nothing, c0') -> d & dComp0 .~ Right c0'
                Left c0' -> d & dComp0 .~ Left c0'
        (v:vs) -> d & dComp1 .~ Right (resume v p1) & dSentBy0 .~ vs
    Right c1 ->
      case step c1 of
        Right (Just v, c1') ->
          d & dTotalSent1 +~ 1 & dSentBy1 %~ (`snoc` v) & dComp1 .~ Right c1'
        Right (Nothing, c1') -> d & dComp1 .~ Right c1'
        Left c1' -> d & dComp1 .~ Left c1'

runDuet :: Duet -> Duet
runDuet d
  | duetStopped d = d
  | otherwise = runDuet (stepDuet d)

parse :: [String] -> Program
parse = map (parseInstr . splitOn " ")
  where
    parseInstr ["snd", src] = Snd (parseExpr src)
    parseInstr ["set", src, dst] = Set (parseExpr src) (parseExpr dst)
    parseInstr ["add", src, dst] = Add (parseExpr src) (parseExpr dst)
    parseInstr ["mul", src, dst] = Mul (parseExpr src) (parseExpr dst)
    parseInstr ["mod", src, dst] = Mod (parseExpr src) (parseExpr dst)
    parseInstr ["jgz", src, dst] = Jgz (parseExpr src) (parseExpr dst)
    parseInstr ["rcv", reg] = Rcv $ parseExpr reg
    parseReg [r] = r
    parseReg e = error $ e ++ " is not a register"
    parseExpr src =
      if head src `elem` registers
        then SRegister $ parseReg src
        else SValue $ read src

readProgram :: IO Program
readProgram = fmap parse readLines

example :: Program
example =
  parse
    [ "set a 1"
    , "add a 2"
    , "mul a a"
    , "mod a 5"
    , "snd a"
    , "set a 0"
    , "rcv a"
    , "jgz a -1"
    , "set a 1"
    , "jgz a -2"
    ]
