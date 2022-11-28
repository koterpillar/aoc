module Y2020.Day19 where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import           AOC
import           Utils

data Rule
  = RChar Char
  | RRecurse [[Int]]
  | RMany Int
  | RBalanced Int Int
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
    go (RBalanced a b) = do
      as <- length <$> many (matcher rules a)
      replicateM_ as (matcher rules b)
    goSeq = traverse_ $ matcher rules

matches :: Rules -> Message -> Bool
matches rules = isRight . runParse (stateP $ matcher rules 0)

data Rule2
  = R2Set Int (Set String)
  | R2Many Rule2
  | R2Balanced Rule2 Rule2
  | R2Seq Rule2 Rule2
  deriving (Eq, Show)

mkr2set :: Set String -> Rule2
mkr2set alts =
  case Set.toList $ Set.map length alts of
    [l] -> R2Set l alts
    ls  -> error $ "mkr2set: lengths differ: " ++ show alts

r2unset :: Rule2 -> Set String
r2unset (R2Set _ s) = s
r2unset r           = error $ "expected R2Set, found: " ++ show r

setConcat :: (Ord el, Monoid el) => Set el -> Set el -> Set el
setConcat a b = Set.map (uncurry mappend) $ Set.cartesianProduct a b

convert :: Rules -> Rule2
convert rules = go Set.empty 0
  where
    go seen idx
      | idx `Set.member` seen = error $ "loop detected: " ++ show idx
      | otherwise =
        case getRule rules idx of
          RChar c -> mkr2set $ Set.singleton [c]
          RRecurse rs ->
            case map (map (go seen')) rs of
              [[a@R2Balanced {}, b@R2Many {}]] -> R2Seq a b
              [[a@R2Many {}, b@R2Balanced {}]] -> R2Seq a b
              rsets ->
                mkr2set $
                Set.unions $ map (foldr1 setConcat . map r2unset) rsets
          RMany i -> R2Many $ go seen' i
          RBalanced a b -> R2Balanced (go seen' a) (go seen' b)
      where
        seen' = Set.insert idx seen

structure :: Rule2 -> Rule2
structure (R2Set l _)      = r2dummy l
structure (R2Seq a b)      = R2Seq (structure a) (structure b)
structure (R2Balanced a b) = R2Balanced (structure a) (structure b)
structure (R2Many a)       = R2Many (structure a)

r2dummy :: Int -> Rule2
r2dummy l = mkr2set $ Set.singleton $ replicate l 'x'

matcher2 :: Rule2 -> StateParser Message ()
matcher2 (R2Set l alts) =
  StateT $ \s ->
    let (h, t) = splitAt l s
     in if Set.member h alts
          then Right ((), t)
          else Left "no match for R2Set"
matcher2 (R2Many r) = void $ many (matcher2 r)
matcher2 (R2Balanced a b) = do
  as <- length <$> many (matcher2 a)
  replicateM_ as (matcher2 b)
matcher2 (R2Seq a b) = matcher2 a >> matcher2 b

matches2 :: Rule2 -> Message -> Bool
matches2 rules = isRight . runParse (stateP $ matcher2 rules)

countValid :: Input -> Int
countValid (rs, msgs) = countIf (matches rs) msgs

fixups :: Rules
fixups = mapFromList [(8, RMany 42), (11, RBalanced 42 31)]

doFixups :: Rules -> Rules
doFixups rs = fixups <> rs

countValid2 :: (Rules -> Rules) -> Input -> Int
countValid2 rulemod (rs, msgs) = countIf (matches2 $ convert $ rulemod rs) msgs

testMessages :: [Message]
testMessages =
  [ "bbabbbbaabaabba"
  , "babbbbaabbbbbabbbbbbaabaaabaaa"
  , "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
  , "bbbbbbbaaaabbbbaaabbabaaa"
  , "bbbababbbbaaaaaaaabbababaaababaabab"
  , "ababaaaaaabaaab"
  , "ababaaaaabbbaba"
  , "baabbaaaabbaaaababbaababb"
  , "abbbbabbbbaaaababbbbbbaaaababb"
  , "aaaaabbaabaaaaababaa"
  , "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
  , "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]

testMessage :: Message
testMessage = testMessages !! 1

tasks :: Tasks
tasks =
  Tasks
    2020
    19
    (CodeBlock 4)
    parser
    [ Task countValid 3
    , Task (structure . convert . fst) (r2dummy 15)
    , Task
        (structure . convert . doFixups . fst)
        (R2Seq (R2Many (r2dummy 5)) (R2Balanced (r2dummy 5) (r2dummy 5)))
    , Task (countValid2 id) 3
    , Task (countValid2 doFixups) 12
    ]
