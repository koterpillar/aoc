module Y2020.Day02 where

import qualified Data.Text as Text

import           AOC
import           Miniparse
import           Utils

data Policy = Policy
  { pOne  :: Int
  , pTwo  :: Int
  , pChar :: Char
  } deriving (Ord, Eq, Show)

parsePolicy :: Parser Text [(Policy, Text)]
parsePolicy = linesP &** tsplitP ": " &* policyP &+ idP
  where
    policyP =
      (\((pOne, pTwo), pChar) -> Policy {..})
        <$> wordsP &* (tsplitP "-" &* integerP &+ integerP) &+ charP

isValid1 :: Policy -> Text -> Bool
isValid1 Policy {..} t = inRange pOne pTwo $ Text.count (Text.singleton pChar) t

isValid2 :: Policy -> Text -> Bool
isValid2 Policy {..} t = (at pOne == pChar) /= (at pTwo == pChar)
  where
    at i = Text.index t (i - 1)

countValid :: (a -> b -> Bool) -> [(a, b)] -> Int
countValid = countIf . uncurry

tasks =
  Tasks
    2020
    2
    (CodeBlock 0)
    parsePolicy
    [Task (countValid isValid1) 2, Task (countValid isValid2) 1]
