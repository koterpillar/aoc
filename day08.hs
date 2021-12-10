import           Control.Monad    (join)

import           Data.Functor

import           Data.Maybe

import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Text.Parsec
import           Text.Parsec.Text

import           AOC
import           Utils

import           Debug.Trace

data Wire
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Ord, Eq, Show)

type Display = Set Wire

data Reading =
  Reading (Set Display) [Display]
  deriving (Ord, Eq, Show)

guessNumbers :: Set Display -> Display -> Maybe Int
guessNumbers _ display
  | Set.size display == 2 = Just 1
  | Set.size display == 4 = Just 4
  | Set.size display == 3 = Just 7
  | Set.size display == 7 = Just 8
  | otherwise = Nothing

readingP :: Parser Reading
readingP =
  Reading <$> (Set.fromList <$> displaysP) <* char '|' <* optional (char '\n') <*
  skipMany spc <*>
  displaysP <*
  char '\n'
  where
    displayP :: Parser Display
    displayP = Set.fromList <$> many1 wireP
    wireP :: Parser Wire
    wireP =
      char 'a' $> A <|> char 'b' $> B <|> char 'c' $> C <|> char 'd' $> D <|>
      char 'e' $> E <|>
      char 'f' $> F <|>
      char 'g' $> G
    displaysP :: Parser [Display]
    displaysP = many1 $ displayP <* optional spc
    spc = char ' '

readingsP :: Parser [Reading]
readingsP = many1 readingP

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

main :: IO ()
main = do
  processEI 8 (justParse readingsP) part1 26
