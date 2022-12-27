module Y2022.Day25 where

import           AOC
import           Utils

newtype Snafu =
  Snafu [Int]
  deriving (Eq, Ord)

snChars = "=-012"

instance Show Snafu where
  show (Snafu xs) = map (\i -> snChars !! (i + 2)) (reverse xs)

snafuDP :: Parser Char Int
snafuDP = choiceP $ zip snChars [-2 .. 2]

parser :: Parser Text [Snafu]
parser = linesP &** (Snafu . reverse <$> charactersP &** snafuDP)

toInt :: Snafu -> Int
toInt (Snafu xs) = foldr (\x r -> x + 5 * r) 0 xs

fromInt :: Int -> Snafu
fromInt = Snafu . unfoldr f
  where
    f 0 = Nothing
    f n =
      let q = wrapRange (-2) 2 n
       in Just (q, (n - q) `div` 5)

part1 = show . fromInt . sum . map toInt

tasks = Tasks 2022 25 (CodeBlock 0) parser [task part1 "2=-1=0"]
