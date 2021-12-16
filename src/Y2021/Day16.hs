module Y2021.Day16 where

import           Control.Monad.State

import           AOC
import           Bit
import           Utils

data Packet
  = Literal Int Int
  | Operator Int Int [Packet]
  deriving (Eq, Show)

ssplitAt :: Int -> State BitString BitString
ssplitAt = state . splitAt

snumber :: Int -> State BitString Int
snumber = fmap bitsValue . ssplitAt

spacket :: State BitString Packet
spacket = do
  version <- snumber 3
  typeID <- snumber 3
  case typeID of
    4 -> Literal version <$> literalBits
    _ -> Operator version typeID <$> operatorPacket

operatorPacket :: State BitString [Packet]
operatorPacket = do
  lengthType <- ssplitAt 1
  case lengthType of
    [O] -> do
      packetsLength <- snumber 15
      currentLength <- gets length
      let remainingLength = currentLength - packetsLength
      packetsUntilLength remainingLength
    [I] -> do
      packetCount <- snumber 11
      replicateM packetCount spacket
    _ -> error "unexpected length type"

packetsUntilLength :: Int -> State BitString [Packet]
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

literalBits :: State BitString Int
literalBits = bitsValue <$> go
  where
    go = do
      bits <- ssplitAt 5
      case bits of
        []     -> error "literalBits: empty list"
        O:rest -> pure rest
        I:rest -> (rest ++) <$> go

mkPacket :: BitString -> Packet
mkPacket = evalState spacket

hexDigitP :: Parser Text Int
hexDigitP = charP &* Parser go
  where
    go c
      | isDigit c = Right $ ord c - ord '0'
      | c <= 'F' && c >= 'A' = Right $ ord c - ord 'A' + 10
      | otherwise = Left $ "Invalid hex digit " <> show c

hexToBits :: [Int] -> BitString
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
part1 (Literal v _)     = v
part1 (Operator v _ ps) = v + sum (map part1 ps)

part2 :: Packet -> Int
part2 (Literal _ v) = v
part2 (Operator _ 0 ps) = sum $ map part2 ps
part2 (Operator _ 1 ps) = product $ map part2 ps
part2 (Operator _ 2 ps) = minimum $ map part2 ps
part2 (Operator _ 3 ps) = maximum $ map part2 ps
part2 (Operator _ 5 [p1, p2]) =
  if part2 p1 > part2 p2
    then 1
    else 0
part2 (Operator _ 6 [p1, p2]) =
  if part2 p1 < part2 p2
    then 1
    else 0
part2 (Operator _ 7 [p1, p2]) =
  if part2 p1 == part2 p2
    then 1
    else 0
part2 _ = error "unexpected operator"

assertPacket :: String -> Packet -> Text -> Task a
assertPacket msg packet input = Assert msg (Right packet) (runParse parse input)

tasks =
  Tasks
    2021
    16
    (pureP ttrim &* parse)
    [ assertPacket "literal packet" (Literal 6 2021) "D2FE28"
    , assertPacket
        "operator packet 1"
        (Operator 1 6 [Literal 6 10, Literal 2 20])
        "38006F45291200"
    , assertPacket
        "operator packet 2"
        (Operator 7 3 [Literal 2 1, Literal 4 2, Literal 1 3])
        "EE00D40C823060"
    , Task part1 31
    , Task part2 54
    ]
