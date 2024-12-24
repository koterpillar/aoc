module Bit where

import           Data.List (unfoldr)

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

bitAnd :: Bit -> Bit -> Bit
bitAnd O _ = O
bitAnd _ O = O
bitAnd _ _ = I

bitOr :: Bit -> Bit -> Bit
bitOr I _ = I
bitOr _ I = I
bitOr _ _ = O

bitXor :: Bit -> Bit -> Bit
bitXor a b = boolToBit $ a /= b

type BitString = [Bit]

bitsValue :: BitString -> Int
bitsValue = foldl bitMult 0
  where
    bitMult n b = n * 2 + fromEnum b

toBitString :: Int -> BitString
toBitString = reverse . unfoldr bitDiv
  where
    bitDiv n
      | n < 0 = error "toBitString: negative number"
      | n == 0 = Nothing
      | otherwise =
        let (q, r) = n `divMod` 2
         in Just (toEnum r, q)
