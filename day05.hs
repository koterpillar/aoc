import           Control.Monad    (join)

import           Text.Parsec      hiding (between, count)
import           Text.Parsec.Text

import           AOC
import           Grid
import           Utils

type VentLine = (Position2, Position2)

ventLineP :: Parser VentLine
ventLineP = (,) <$> position2P <* string " -> " <*> position2P
  where
    position2P = Position2 <$> integerP <* string "," <*> integerP

vlBounds :: [VentLine] -> (Position2, Position2)
vlBounds = bounds . join . map (\(p1, p2) -> [p1, p2])

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= b && b <= c || a >= b && b >= c

vlOn :: Position2 -> VentLine -> Bool
vlOn (Position2 x y) (Position2 x1 y1, Position2 x2 y2)
  | x1 == x2 = x == x1 && between y1 y y2
  | y1 == y2 = y == y1 && between x1 x x2
  | otherwise = False

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

countOnLines :: (Position2 -> VentLine -> Bool) -> [VentLine] -> Int
countOnLines onPred vlines = count (> 1) $ map countLines allPoints
  where
    (Position2 xmin ymin, Position2 xmax ymax) = vlBounds vlines
    allPoints = [Position2 x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]
    countLines pt = count (onPred pt) vlines

part1 :: [VentLine] -> Int
part1 = countOnLines vlOn

vlOn2 :: Position2 -> VentLine -> Bool
vlOn2 (Position2 x y) (Position2 x1 y1, Position2 x2 y2) =
  between x1 x x2 && between y1 y y2 && sameAngle x1 x x2 y1 y y2

sameAngle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
sameAngle a1 a2 a3 b1 b2 b3 = da1 * db2 == db1 * da2
  where
    da1 = a2 - a1
    da2 = a3 - a2
    db1 = b2 - b1
    db2 = b3 - b2

part2 :: [VentLine] -> Int
part2 = countOnLines vlOn2

main :: IO ()
main = do
  assertEqual "On point" True $
    vlOn (Position2 2 4) (Position2 2 5, Position2 2 3)
  processEI 5 (parseLines ventLineP) part1 5
  processEI 5 (parseLines ventLineP) part2 12