module Y2022.Day13 where

import           AOC
import           Utils

data Packet
  = PNum Int
  | PList [Packet]
  deriving (Show)

packetP :: Parser Text Packet
packetP = charactersP &* stateP packetSP

packetSP :: StateParser [Char] Packet
packetSP = do
  unconsSP_ >>= \case
    '[' -> PList <$> listSP
    c
      | isDigit c -> do
        putBackSP c
        PNum <$> naturalSP
      | otherwise -> failSP $ "packetSP: unexpected character: " <> [c]

listSP :: StateParser [Char] [Packet]
listSP = do
  unconsSP_ >>= \case
    ']' -> pure []
    c -> do
      putBackSP c
      p <- packetSP
      unconsSP_ >>= \case
        ',' -> (p :) <$> listSP
        ']' -> pure [p]

type Input = [(Packet, Packet)]

parser :: Parser Text Input
parser = lineGroupsP &** (packetP &+ packetP)

packetNumToList :: Int -> Packet
packetNumToList n = PList [PNum n]

instance Eq Packet where
  a == b = compare a b == EQ

instance Ord Packet where
  compare (PNum a) (PNum b)   = a `compare` b
  compare (PList a) (PList b) = a `compare` b
  compare (PNum a) (PList b)  = compare (packetNumToList a) (PList b)
  compare (PList a) (PNum b)  = compare (PList a) (packetNumToList b)

part1 :: Input -> Int
part1 = sum . map fst . filter ((/= GT) . uncurry compare . snd) . zip [1 ..]

dividers :: [Packet]
dividers = [PList [PList [PNum n]] | n <- [2, 6]]

part2 :: Input -> Int
part2 =
  product .
  map fst .
  filter (\(_, p) -> p `elem` dividers) .
  zip [1 ..] . sort . (++) dividers . concatMap (\(a, b) -> [a, b])

tasks = Tasks 2022 13 (CodeBlock 0) parser [Task part1 13, Task part2 140]
