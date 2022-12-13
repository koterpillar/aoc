module Y2021.Day08 where

import           Data.Foldable

import           AOC
import           Utils

data Wire
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Ord, Eq, Show, Enum, Bounded)

wires :: [Wire]
wires = enumerate

type Display = Set Wire

data Reading =
  Reading (Set Display) [Display]
  deriving (Ord, Eq, Show)

guessNumbers :: Set Display -> Display -> Maybe Int
guessNumbers allnumbers = flip mapLookup result
  where
    findBySize n cs =
      case find (\x -> length x == n) cs of
        Nothing -> error $ "Cannot find set with " <> show n <> " elements"
        Just c  -> c
    n1 = findBySize 2 allnumbers
    n4 = findBySize 4 allnumbers
    n7 = findBySize 3 allnumbers
    n8 = findBySize 7 allnumbers
    single :: (Foldable f, Show a) => String -> f a -> a
    single msg s =
      case toList s of
        [x]   -> x
        other -> error $ "Cannot choose " <> msg <> " from " <> show other
    a = single "a" $ setDifference n7 n1
    happensIn = happensIn' allnumbers
    happensIn' displays n =
      single ("occurs " <> show n <> " times") $ happenIn displays n
    happenIn displays n =
      filter (\w -> countIf (setMember w) (toList displays) == n) wires
    b = happensIn 6
    e = happensIn 4
    f = happensIn 9
    c = single "c" $ setDifference n1 (set1 f)
    d = single "d" $ setDifference n4 $ n1 <> set1 b
    g =
      single "g" $
      setDifference (setFromList wires) (setFromList [a, b, c, d, e, f])
    n0 = setFromList [a, b, c, e, f, g]
    n2 = setFromList [a, c, d, e, g]
    n3 = setFromList [a, c, d, f, g]
    n5 = setFromList [a, b, d, f, g]
    n6 = setFromList [a, b, d, e, f, g]
    n9 = setFromList [a, b, c, d, f, g]
    result =
      mapFromList
        [ (n0, 0)
        , (n1, 1)
        , (n2, 2)
        , (n3, 3)
        , (n4, 4)
        , (n5, 5)
        , (n6, 6)
        , (n7, 7)
        , (n8, 8)
        , (n9, 9)
        ]

readDisplay :: Reading -> Int
readDisplay (Reading allnumbers digits) =
  foldl (\n d -> n * 10 + d) 0 digitsValues
  where
    guess = guessNumbers allnumbers
    digitsValues = map (fromJust . guess) digits

wireP :: Parser Char Wire
wireP = choiceEBP ['a' .. 'g']

displayP :: Parser Text Display
displayP = setFromList <$> charactersP &** wireP

displaysP :: Parser Text [Display]
displaysP = wordsP &** displayP

readingP :: Parser Text Reading
readingP =
  tsplitP "|" &* pairPWith Reading (setFromList <$> displaysP) displaysP

readingsP :: Parser Text [Reading]
readingsP = pureP (treplace "|\n" "|") &* linesP &** readingP

part1 :: [Reading] -> Int
part1 = sum . map countSimple
  where
    countSimple :: Reading -> Int
    countSimple (Reading rall rdisp) =
      let guess = guessNumbers rall
       in countIf isSimple $ mapMaybe guess rdisp
    isSimple :: Int -> Bool
    isSimple 1 = True
    isSimple 4 = True
    isSimple 7 = True
    isSimple 8 = True
    isSimple _ = False

part2 :: [Reading] -> Int
part2 = sum . map readDisplay

tasks = Tasks 2021 8 (CodeBlock 2) readingsP [Task part1 26, Task part2 61229]
