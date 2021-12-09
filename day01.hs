import           AOC
import           Data.List
import           Text.Parsec
import           Utils

countTrue = length . filter id

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
  example <- readParse integerP (Example 1)
  print $ part1 example
  print $ part2 example
  input <- readParse integerP (Input 1)
  print $ part1 input
  print $ part2 input
