module Y2020.Day04 where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Utils

type Passport = Map Text Text

parsePassports :: Parser Text [Passport]
parsePassports = lineGroupsP &** parsePassport

parsePassport :: Parser [Text] Passport
parsePassport =
  Map.fromList <$>
  pureP (Text.intercalate " ") &* wordsP &** (tsplitP ":" &* (idP &+ idP))

isValid :: Passport -> Bool
isValid p = all (`Map.member` p) requiredElems

requiredElems = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

countValid = length . filter isValid

tasks = Tasks 2020 4 (CodeBlock 0) parsePassports [Task countValid 2]
