module Bit where

data Bit
  = O
  | I
  deriving (Ord, Eq, Show)

bitInvert :: Bit -> Bit
bitInvert O = I
bitInvert I = O

type BitString = [Bit]

bitsValue :: BitString -> Int
bitsValue = foldl bitMult 0
  where
    bitMult n O = n * 2
    bitMult n I = n * 2 + 1
