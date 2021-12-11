import           AOC
import           Text.Parsec
import           Utils

zipWithTail fn values = zipWith fn values (tail values)

part1 = countTrue . zipWithTail (<)

take' n xs =
  let result = take n xs
   in if length result == n
        then pure result
        else mempty

slidingSum n = map sum . (=<<) (take' n) . tails

part2 = countTrue . zipWithTail (<) . slidingSum 3

main = do
  processEI 1 (parseLines integerP) part1 7
  processEI 1 (parseLines integerP) part2 5
