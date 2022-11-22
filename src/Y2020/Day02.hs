module Y2020.Day02 where

import qualified Data.Text as Text

import           AOC
import           Miniparse
import           Utils

data Policy =
  Policy
    { pOne  :: Int
    , pTwo  :: Int
    , pChar :: Char
    }
  deriving (Ord, Eq, Show)

parsePolicy :: Parser Text [(Policy, Text)]
parsePolicy = linesP &** (tsplitP ": " &* pairP &* (policyP &= idP))
  where
    policyP =
      (\((pOne, pTwo), pChar) -> Policy {..}) <$>
      wordsP &* pairP &*
      ((tsplitP "-" &* pairP &* (integerP &= integerP)) &= charP)

isValid1 :: Policy -> Text -> Bool
isValid1 Policy {..} t =
  let n = Text.count (Text.singleton pChar) t
   in pOne <= n && n <= pTwo

isValid2 :: Policy -> Text -> Bool
isValid2 Policy {..} t = (at pOne == pChar) /= (at pTwo == pChar)
  where
    at i = Text.index t (i - 1)

countValid :: (a -> b -> Bool) -> [(a, b)] -> Int
countValid validator = length . filter (uncurry validator)

tasks =
  Tasks
    2020
    02
    (CodeBlock 0)
    parsePolicy
    [Task (countValid isValid1) 2, Task (countValid isValid2) 1]
