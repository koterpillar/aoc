module Y2022.Day13 where

import           Data.Aeson

import           AOC
import           Utils

data Packet
  = PNum Int
  | PList [Packet]
  deriving (Show)

instance FromJSON Packet where
  parseJSON p = PNum <$> parseJSON p <|> PList <$> parseJSON p

type Input = [(Packet, Packet)]

parser :: Parser Text Input
parser = lineGroupsP &** (jsonP &+ jsonP)

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
part1 = sum . filterTuple (uncurry (<=)) . zipN 1

dividers :: [Packet]
dividers = [PList [PList [PNum n]] | n <- [2, 6]]

part2 :: Input -> Int
part2 =
  product .
  filterTuple (`elem` dividers) .
  zipN 1 . sort . (++) dividers . concatMap (\(a, b) -> [a, b])

tasks =
  Tasks
    2022
    13
    (CodeBlock 0)
    parser
    [ Assert
        "nesting matters"
        LT
        (let x = PNum 2
          in compare (PList [x, PList [x, x]]) (PList [PList [x, x], x]))
    , Task part1 13
    , Task part2 140
    ]
