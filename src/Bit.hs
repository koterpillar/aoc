module Bit where

data Bit
  = O
  | I
  deriving (Ord, Eq, Show, Enum, Bounded)

bitToBool :: Bit -> Bool
bitToBool O = False
bitToBool I = True

boolToBit :: Bool -> Bit
boolToBit False = O
boolToBit True  = I

bitInvert :: Bit -> Bit
bitInvert = boolToBit . not . bitToBool

type BitString = [Bit]

bitsValue :: BitString -> Int
bitsValue = foldl bitMult 0
  where
    bitMult n O = n * 2
    bitMult n I = n * 2 + 1
