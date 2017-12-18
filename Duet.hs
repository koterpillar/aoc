{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Duet where

import Control.Lens

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
  , _cSound :: Maybe Value
  , _cProgram :: Program
  , _cIP :: Address
  } deriving (Eq, Ord, Show)

makeLenses ''Computer

registers :: [Register]
registers = ['a' .. 'z']

boot :: Program -> Computer
boot instructions = Computer (M.fromList $ zip registers (repeat 0)) Nothing instructions 0

stopped :: Computer -> Bool
stopped (Computer _ _ program ip) = ip < 0 || ip >= length program

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

run :: Computer -> Computer
run = head . dropWhile (not . stopped) . iterate step

step :: Computer -> Computer
step c = go (c ^. cInstruction) c
  where
    go (Snd x) = cSound .~ Just (eval c x)
    go (Set (SRegister x) y) = cAdvance . (cRegister x .~ eval c y)
    go (Add (SRegister x) y) = cAdvance . (cRegister x +~ eval c y)
    go (Mul (SRegister x) y) = cAdvance . (cRegister x *~ eval c y)
    go (Mod (SRegister x) y) = cAdvance . (cRegister x %~ (`mod` eval c y))
    go (Rcv (SRegister x)) = cAdvance . rcv
      where
        rcv =
          case c ^. cRegister x of
            0 -> id
            _ -> cRegister x .~ fromJust (c ^. cSound)
    go (Jgz x y) =
      if eval c x > 0
        then cIP +~ eval c y
        else cAdvance

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
