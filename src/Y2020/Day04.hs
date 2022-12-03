module Y2020.Day04 where

import qualified Data.Text as Text

import           AOC
import           Utils

type Passport = Map Text Text

parsePassports :: Parser Text [Passport]
parsePassports = lineGroupsP &** parsePassport

parsePassport :: Parser [Text] Passport
parsePassport =
  mapFromList <$> pureP Text.unwords &* wordsP &** tsplitP ":" &* pairP

isValid :: Passport -> Bool
isValid p = all (`mapMember` p) requiredElems

requiredElems = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

fieldParser :: Text -> Parser Text a -> Parser Passport ()
fieldParser k fp = lookupP k &* fp &* constP ()

rangeP :: (Ord k, Show k) => k -> k -> Parser k k
rangeP lo hi = filterP $ inRange lo hi

yearParser :: Int -> Int -> Parser Text Int
yearParser ymin ymax =
  filterP (\t -> tlength t == 4) &* integerP &* rangeP ymin ymax

byr = fieldParser "byr" $ yearParser 1920 2002

iyr = fieldParser "iyr" $ yearParser 2010 2020

eyr = fieldParser "eyr" $ yearParser 2020 2030

hgt =
  fieldParser "hgt" $
  tspanP isDigit &* integerP &= idP &* filterP (uncurry valid . swap)
  where
    valid "cm" = inRange 150 193
    valid "in" = inRange 59 76
    valid _    = const False

hcl = fieldParser "hcl" $ tspanP (== '#') &* filterP (uncurry valid)
  where
    valid "#" t = tlength t == 6 && Text.all isHexDigit t
    valid _ _   = False
    isHexDigit c = isDigit c || inRange 'a' 'f' c

ecl =
  fieldParser "ecl" $
  filterP (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

pid = fieldParser "pid" $ digitsP &* filterP (\t -> length t == 9)

betterValid :: Passport -> Bool
betterValid passport =
  all isRight [runParse p passport | p <- [byr, iyr, eyr, hgt, hcl, ecl, pid]]

tasks =
  Tasks
    2020
    4
    (CodeBlock 0)
    parsePassports
    [Task (countIf isValid) 2, Task (countIf betterValid) 2]
