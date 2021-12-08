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
  depths <- readParse integerP
  print $ part1 depths
  print $ part2 depths
