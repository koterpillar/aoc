{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens

import           Data.Either

import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe

import           Utils

type Register = Char

type Value = Int

data Expr
  = SRegister Register
  | SValue Value
  deriving (Eq, Ord, Show)

type Offset = Int

data Instruction
  = Set Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Jnz Expr Expr
  deriving (Ord, Eq, Show)

type Registers = M.Map Register Value

type Program = [Instruction]

type Address = Int

data Computer =
  Computer
    { _cRegisters   :: Registers
    , _cProgram     :: Program
    , _cIP          :: Address
    , _cMulExecuted :: Int
    }
  deriving (Eq, Ord, Show)

makeLenses ''Computer

registers :: [Register]
registers = ['a' .. 'h']

boot :: Program -> Computer
boot instructions = Computer {..}
  where
    _cRegisters = M.fromList $ zip registers (repeat 0)
    _cProgram = instructions
    _cIP = 0
    _cMulExecuted = 0

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
    (SValue val)    -> val
    (SRegister reg) -> comp ^. cRegister reg

step :: Computer -> Computer
step c = go (c ^. cInstruction) c
  where
    go (Set (SRegister x) y) = cAdvance . (cRegister x .~ eval c y)
    go (Sub (SRegister x) y) = cAdvance . (cRegister x -~ eval c y)
    go (Mul (SRegister x) y) =
      cAdvance . (cRegister x *~ eval c y) . (cMulExecuted +~ 1)
    go (Jnz x y) =
      if eval c x /= 0
        then cIP +~ eval c y
        else cAdvance

parse :: [String] -> Program
parse = map (parseInstr . splitOn " ")
  where
    parseInstr ["set", src, dst] = Set (parseExpr src) (parseExpr dst)
    parseInstr ["sub", src, dst] = Sub (parseExpr src) (parseExpr dst)
    parseInstr ["mul", src, dst] = Mul (parseExpr src) (parseExpr dst)
    parseInstr ["jnz", src, dst] = Jnz (parseExpr src) (parseExpr dst)
    parseReg [r] = r
    parseReg e   = error $ e ++ " is not a register"
    parseExpr src =
      if head src `elem` registers
        then SRegister $ parseReg src
        else SValue $ read src

readProgram :: IO Program
readProgram = fmap parse readLines
