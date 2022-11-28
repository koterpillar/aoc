module Y2020.Day19 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Rule
  = RChar Char
  | RRecurse [[Int]]
  | RMany Int
  | RManyNested Int Int
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
    go (RChar c) = unconsSP_ >>= guard . (== c)
    go (RRecurse rs) = foldr1 (<|>) $ map goSeq rs
    go (RMany i) = void $ many (matcher rules i)
    go (RManyNested a b) = do
      as <- length <$> many (matcher rules a)
      replicateM_ as (matcher rules b)
    goSeq = traverse_ $ matcher rules

matches :: Rules -> Message -> Bool
matches rules = isRight . runParse (stateP $ matcher rules 0)

allMatching :: Rules -> Int -> Set Message
allMatching rules idx = go (getRule rules idx)
  where
    go (RChar c)     = Set.singleton [c]
    go (RRecurse rs) = mconcat $ map goSeq rs
    go x             = error $ show x
    goSeq =
      foldr1 (\a b -> Set.map (uncurry (++)) $ Set.cartesianProduct a b) .
      map (allMatching rules)

countValid :: Input -> Int
countValid (rs, msgs) = countIf (matches rs) msgs

fixups :: Rules
fixups = mapFromList [(8, RMany 42), (11, RManyNested 42 31)]

recursionIsEasy :: Input -> Set Message
recursionIsEasy (rules, _) = a `Set.intersection` b
  where
    a = allMatching rules 31
    b = allMatching rules 42

-- 42 is this
easySet42 :: Set Message
easySet42 =
  Set.fromList
    [ "aaaaa"
    , "aaaab"
    , "aaaba"
    , "aaabb"
    , "aabbb"
    , "ababa"
    , "abbbb"
    , "baaaa"
    , "baabb"
    , "babbb"
    , "bbaaa"
    , "bbaab"
    , "bbabb"
    , "bbbab"
    , "bbbba"
    , "bbbbb"
    ]

-- 31 is this
easySet31 :: Set Message
easySet31 =
  Set.fromList
    [ "aabaa"
    , "aabab"
    , "aabba"
    , "abaaa"
    , "abaab"
    , "ababb"
    , "abbaa"
    , "abbab"
    , "abbba"
    , "baaab"
    , "baaba"
    , "babaa"
    , "babab"
    , "babba"
    , "bbaba"
    , "bbbaa"
    ]

doFixups :: Rules -> Rules
doFixups rs = fixups <> rs

countValidFixups :: Input -> Int
countValidFixups (rs, msgs) = countValid (doFixups rs, msgs)

startRule :: Input -> Rule
startRule = fromJust . Map.lookup 0 . fst

tasks =
  Tasks
    2020
    19
    (CodeBlock 4)
    parser
    [ Task countValid 3
    , AssertExample "all matching 31" easySet31 (flip allMatching 31 . fst)
    , AssertExample "all matching 42" easySet42 (flip allMatching 42 . fst)
    , Task recursionIsEasy Set.empty
    , Task startRule (RRecurse [[8, 11]])
    , AssertExample
        "message 3 matches"
        True
        (flip matches "babbbbaabbbbbabbbbbbaabaaabaaa" . doFixups . fst)
    , Task countValidFixups 12
    ]
