module Y2021.Day08 where

import           Data.Foldable

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text     as Text

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
wires = boundedAll

type Display = Set Wire

data Reading =
  Reading (Set Display) [Display]
  deriving (Ord, Eq, Show)

guessNumbers :: Set Display -> Display -> Maybe Int
guessNumbers allnumbers = flip Map.lookup result
  where
    findBySize n cs =
      case find (\x -> Set.size x == n) cs of
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
    a = single "a" $ Set.difference n7 n1
    happensIn = happensIn' allnumbers
    happensIn' displays n =
      single ("occurs " <> show n <> " times") $ happenIn displays n
    happenIn displays n =
      filter (\w -> countIf (Set.member w) (Set.toList displays) == n) wires
    b = happensIn 6
    e = happensIn 4
    f = happensIn 9
    c = single "c" $ Set.difference n1 (Set.singleton f)
    d = single "d" $ Set.difference n4 $ Set.union n1 $ Set.singleton b
    g =
      single "g" $
      Set.difference (Set.fromList wires) (Set.fromList [a, b, c, d, e, f])
    n0 = Set.fromList [a, b, c, e, f, g]
    n2 = Set.fromList [a, c, d, e, g]
    n3 = Set.fromList [a, c, d, f, g]
    n5 = Set.fromList [a, b, d, f, g]
    n6 = Set.fromList [a, b, d, e, f, g]
    n9 = Set.fromList [a, b, c, d, f, g]
    result =
      Map.fromList
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
wireP =
  choiceP [('a', A), ('b', B), ('c', C), ('d', D), ('e', E), ('f', F), ('g', G)]

displayP :: Parser Text Display
displayP = Set.fromList <$> charactersP &** wireP

displaysP :: Parser Text [Display]
displaysP = wordsP &** displayP

readingP :: Parser Text Reading
readingP =
  tsplitP "|" &* pairPWith Reading (Set.fromList <$> displaysP) displaysP

readingsP :: Parser Text [Reading]
readingsP = pureP (Text.replace "|\n" "|") &* linesP &** readingP

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

tasks = Tasks 2021 8 readingsP [Task part1 26, Task part2 61229]
