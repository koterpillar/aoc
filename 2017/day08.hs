{-# LANGUAGE RecordWildCards #-}

import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

type Register = String

type Value = Int

data Instr = Instr
  { iReg :: Register
  , iInc :: Value
  , iConditionReg :: Register
  , iConditionFunc :: Value -> Bool
  }

parseInstr :: String -> Instr
parseInstr str = Instr {..}
  where
    [iReg, incdec, incdecvalstr, "if", iConditionReg, condstr, condvalstr] =
      words str
    incdecval = read incdecvalstr
    iInc =
      case incdec of
        "inc" -> incdecval
        "dec" -> negate incdecval
    condval = read condvalstr
    iConditionFunc =
      case condstr of
        ">" -> (> condval)
        "<" -> (< condval)
        ">=" -> (>= condval)
        "<=" -> (<= condval)
        "==" -> (== condval)
        "!=" -> (/= condval)

readInstrs :: IO [Instr]
readInstrs = map parseInstr <$> readLines

type Computer = Map Register Value

cGet :: Computer -> Register -> Value
cGet c r = fromMaybe 0 $ Map.lookup r c

cPut :: Computer -> Register -> Value -> Computer
cPut c r v = Map.insert r v c

process :: Computer -> Instr -> Computer
process c Instr {..} =
  if iConditionFunc (cGet c iConditionReg)
    then cPut c iReg (cGet c iReg + iInc)
    else c

processAll :: Computer -> [Instr] -> Computer
processAll = foldl' process

cMax :: Computer -> Value
cMax = maximum . (0 :) . Map.elems
