module Y2020.Day19 where

import qualified Data.Text as Text

import           AOC
import           Utils

data Rule
  = RChar Char
  | RRecurse [[Int]]
  deriving (Eq, Show)

ruleP :: Parser Text Rule
ruleP =
  (RChar <$>
   filterP (Text.isPrefixOf "\"") &* pureP (Text.replace "\"" "") &* charP) &|
  (RRecurse <$> (tsplitP " | " &** (wordsP &** integerP)))

type Rules = Map Int Rule

rulesP :: Parser [Text] Rules
rulesP = mapFromList <$> traverseP (tsplitP ": " &* (integerP &+ ruleP))

type Message = [Char]

type Input = (Rules, [Message])

parser :: Parser Text Input
parser = lineGroupsP &* (rulesP &+ traverseP stringP)

getRule :: Rules -> Int -> Rule
getRule rules idx =
  case mapLookup idx rules of
    Just r  -> r
    Nothing -> error $ "rule not found: " ++ show idx

matcher :: Rules -> Int -> StateParser Message ()
matcher rules idx = go (getRule rules idx)
  where
    go (RChar c)           = unconsSP_ >>= guard . (== c)
    go (RRecurse [r1])     = goSeq r1
    go (RRecurse [r1, r2]) = goSeq r1 <|> goSeq r2
    go x                   = error $ show x
    goSeq = traverse_ $ matcher rules

matches :: Rules -> Message -> Bool
matches rules = isRight . runParse (stateP $ matcher rules 0)

countValid (rs, msgs) = countIf (matches rs) msgs

fixups =
  mapFromList
    [(8, RRecurse [[42], [42, 8]]), (11, RRecurse [[42, 31], [42, 11, 31]])]

countValidFixups (rs, msgs) = countValid (fixups <> rs, msgs)

tasks =
  Tasks
    2020
    19
    (CodeBlock 2)
    parser
    [ Task countValid 2
    , AssertExample
        "message 3 matches"
        True
        (flip matches "babbbbaabbbbbabbbbbbaabaaabaaa" . fst)
    , TaskScraper (CodeBlock 4) countValidFixups 12
    ]
