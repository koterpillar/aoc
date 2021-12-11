import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Grid
import           Utils

type Movement = (Direction4, Int)

directionP :: Parser Direction4
directionP =
  string "forward " *> pure E <|> string "down " *> pure S <|>
  string "up " *> pure N

movementP :: Parser Movement
movementP = (,) <$> directionP <*> integerP

moves1 :: [Movement] -> Position2
moves1 = foldl (\pos (dir, n) -> walkN n dir pos) (Position2 0 0)

part1 input =
  let (Position2 x y) = moves1 input
   in x * y

data Submarine =
  Submarine Int Position2
  deriving (Show)

moves2 :: [Movement] -> Submarine
moves2 = foldl move2 (Submarine 0 (Position2 0 0))

move2 :: Submarine -> Movement -> Submarine
move2 (Submarine aim (Position2 x y)) (E, n) =
  Submarine aim (Position2 (x + n) (y + aim * n))
move2 (Submarine aim pos) (N, n) = Submarine (aim - n) pos
move2 (Submarine aim pos) (S, n) = Submarine (aim + n) pos

part2 input =
  let (Submarine _ (Position2 x y)) = moves2 input
   in x * y

main = do
  processEI 2 (parseLines movementP) part1 150
  processEI 2 (parseLines movementP) part2 900
