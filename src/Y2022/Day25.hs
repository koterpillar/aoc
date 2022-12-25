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
toInt (Snafu [])     = 0
toInt (Snafu (x:xs)) = x + 5 * toInt (Snafu xs)

fromInt :: Int -> Snafu
fromInt 0 = Snafu []
fromInt n = Snafu $ q : rest
  where
    q = wrapRange (-2) 2 $ n `mod` 5
    r = (n - q) `div` 5
    (Snafu rest) = fromInt r

part1 = show . fromInt . sum . map toInt

tasks = Tasks 2022 25 (CodeBlock 0) parser [Task part1 "2=-1=0"]
