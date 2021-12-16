module Y2021.Day16 where

import           Control.Monad.State

import           AOC
import           Utils

data Bit
  = O
  | I
  deriving (Eq, Ord, Show)

bitsToInt :: [Bit] -> Int
bitsToInt = go . reverse
  where
    go []     = 0
    go (O:xs) = go xs * 2
    go (I:xs) = go xs * 2 + 1

data Packet
  = Literal Int Int
  | Operator Int [Packet]
  deriving (Eq, Show)

ssplitAt :: Int -> State [Bit] [Bit]
ssplitAt = state . splitAt

spacket :: State [Bit] Packet
spacket = do
  version <- bitsToInt <$> ssplitAt 3
  typeID <- bitsToInt <$> ssplitAt 3
  case typeID of
    4 -> Literal version <$> literalBits
    _ -> Operator version <$> operatorPacket

operatorPacket :: State [Bit] [Packet]
operatorPacket = do
  lengthType <- ssplitAt 1
  case lengthType of
    [O] -> do
      packetsLength <- bitsToInt <$> ssplitAt 15
      currentLength <- gets length
      let remainingLength = currentLength - packetsLength
      packetsUntilLength remainingLength
    [I] -> do
      packetCount <- bitsToInt <$> ssplitAt 11
      replicateM packetCount spacket
    _ -> error "unexpected length type"

packetsUntilLength :: Int -> State [Bit] [Packet]
packetsUntilLength remainingLength = do
  currentLength <- gets length
  case currentLength of
    l
      | l < remainingLength -> error "unexpected length"
      | l == remainingLength -> return []
      | otherwise -> do
        packet <- spacket
        packets <- packetsUntilLength remainingLength
        return (packet : packets)

literalBits :: State [Bit] Int
literalBits = bitsToInt <$> go
  where
    go = do
      bits <- ssplitAt 5
      case bits of
        []     -> error "literalBits: empty list"
        O:rest -> pure rest
        I:rest -> (rest ++) <$> go

mkPacket :: [Bit] -> Packet
mkPacket = evalState spacket

hexDigitP :: Parser Text Int
hexDigitP = charP &* Parser go
  where
    go c
      | isDigit c = Right $ ord c - ord '0'
      | c <= 'F' && c >= 'A' = Right $ ord c - ord 'A' + 10
      | otherwise = Left $ "Invalid hex digit " <> show c

hexToBits :: [Int] -> [Bit]
hexToBits = concatMap (htb 4)
  where
    htb n x = htb' n x []
    htb' 0 _ r = r
    htb' n x r =
      htb'
        (n - 1)
        (x `div` 2)
        ((if even x
            then O
            else I) :
         r)

parse :: Parser Text Packet
parse = mkPacket . hexToBits <$> charactersP &** hexDigitP

part1 :: Packet -> Int
part1 = undefined

assertPacket :: String -> Packet -> Text -> Task a
assertPacket msg packet input = Assert msg (Right packet) (runParse parse input)

tasks =
  Tasks
    2021
    16
    parse
    [ assertPacket "literal packet" (Literal 6 2021) "D2FE28"
    , assertPacket
        "operator packet 1"
        (Operator 1 [Literal 6 10, Literal 2 20])
        "38006F45291200"
    , assertPacket
        "operator packet 2"
        (Operator 7 [Literal 2 1, Literal 4 2, Literal 1 3])
        "EE00D40C823060"
    , Task part1 31
    ]
