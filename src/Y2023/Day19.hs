module Y2023.Day19 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Memo
import qualified Ranges
import           Ranges    (Ranges)
import           Utils

data Category
  = X
  | M
  | A
  | S
  deriving (Ord, Eq, Show, Bounded, Enum)

instance Hashable Category where
  hashWithSalt x = hashWithSalt x . fromEnum

data Sign
  = LessThan
  | GreaterThan
  deriving (Ord, Eq, Show, Bounded, Enum)

instance Hashable Sign where
  hashWithSalt x = hashWithSalt x . fromEnum

data Expr =
  Expr Category Sign Int
  deriving (Ord, Eq, Show)

instance Hashable Expr where
  hashWithSalt x (Expr c s i) = hashWithSalt x (c, s, i)

data Workflow
  = Condition Expr Workflow
  | Goto Text
  | Accept
  | Reject
  deriving (Ord, Eq, Show)

instance Hashable Workflow where
  hashWithSalt x (Condition e w) = hashWithSalt x (1 :: Int, e, w)
  hashWithSalt x (Goto i)        = hashWithSalt x (2 :: Int, i)
  hashWithSalt x Accept          = hashWithSalt x (3 :: Int)
  hashWithSalt x Reject          = hashWithSalt x (4 :: Int)

type Workflows = Map Text [Workflow]

type Part = Map Category Int

rating :: Category -> Part -> Int
rating = mapLookupE "rating"

totalRating :: Part -> Int
totalRating = sum . Map.elems

categoryP :: Parser Char Category
categoryP = choiceEBP "xmas"

partP :: Parser Text Part
partP =
  (pureP (Text.drop 1 . Text.dropEnd 1)
     &* tsplitP ","
     &** (tsplitP "=" &* ((charP &* categoryP) &+ integerP)))
    &* pureP Map.fromList

exprP :: Parser Text Expr
exprP =
  uncurryAgain Expr
    <$> (charactersP
           &* (unconsP
                 &* (categoryP
                       &= (unconsP
                             &* (choiceEBP "<>" &= (pureP Text.pack &* integerP))))))

workflowP :: Parser Text Workflow
workflowP =
  (Accept <$ requireP "A")
    &| (Reject <$ requireP "R")
    &| (tsplitP ":" &* ap2P Condition exprP workflowP)
    &| (Goto <$> idP)

workflowsP :: Parser [Text] Workflows
workflowsP =
  traverseP
    (tsplitP "{"
       &* (idP &+ (pureP (Text.dropEnd 1) &* (tsplitP "," &** workflowP))))
    &* pureP Map.fromList

parser :: Parser Text (Workflows, [Part])
parser = lineGroupsP &* (workflowsP &+ traverseP partP)

exprEval :: Expr -> Part -> Bool
exprEval (Expr c s v) p = rating c p `signOp` v
  where
    signOp = op s
    op LessThan    = (<)
    op GreaterThan = (>)

accepts1 :: Workflow -> Part -> Maybe (Either Bool Text)
accepts1 Accept _ = Just $ Left True
accepts1 Reject _ = Just $ Left False
accepts1 (Goto x) _ = Just $ Right x
accepts1 (Condition e w) p
  | exprEval e p = accepts1 w p
  | otherwise = Nothing

accepts2 :: [Workflow] -> Part -> Either Bool Text
accepts2 [] p = error $ "Part " <> show p <> " not accepted by any workflow"
accepts2 (w:ws) p =
  case accepts1 w p of
    Just r  -> r
    Nothing -> accepts2 ws p

accepts :: Text -> Workflows -> Part -> Bool
accepts i w p =
  case accepts2 ws p of
    Left r   -> r
    Right i' -> accepts i' w p
  where
    ws = mapLookupE "accepts" i w

part1 :: Workflows -> [Part] -> Int
part1 w = sum . map totalRating . filter (accepts "in" w)

type MetaPart = Map Category Ranges

metaPartAll :: MetaPart
metaPartAll = Map.fromList [(c, Ranges.singleton 1 4000) | c <- enumerate]

metaPartCount :: MetaPart -> Int
metaPartCount = product . map Ranges.lengthInclusive . toList

exprNegate :: Expr -> Expr
exprNegate (Expr c LessThan v)    = Expr c GreaterThan $ pred v
exprNegate (Expr c GreaterThan v) = Expr c LessThan $ succ v

metaCondition :: Expr -> MetaPart -> MetaPart
metaCondition (Expr c s v) = Map.adjust (go s) c
  where
    go LessThan    = Ranges.subtract (Ranges.singleton (pred v) 4000)
    go GreaterThan = Ranges.subtract (Ranges.singleton 1 $ succ v)

metaAccepts :: Workflows -> Text -> Int
metaAccepts = metaAccepts' metaPartAll

metaAccepts' :: MetaPart -> Workflows -> Text -> Int
metaAccepts' p w i = go p $ mapLookupE "metaAccepts.go'" i w
  where
    go _ [] = error "empty workflow"
    go p (Accept:_) = metaPartCount p
    go _ (Reject:_) = 0
    go p (Goto i':_) = metaAccepts' p w i'
    go p (Condition e w:ws) = r1 + r2
      where
        p1 = metaCondition e p
        p2 = metaCondition (exprNegate e) p
        r1 = go p1 (w : ws)
        r2 = go p2 ws

part2 :: Workflows -> Int
part2 = flip metaAccepts "in"

testWorkflows :: Workflows
testWorkflows =
  Map.fromList
    [ ("hax", [Condition (Expr X GreaterThan 2000) Accept, Reject])
    , ("hbx", [Condition (Expr X LessThan 2001) Accept, Reject])
    , ("h1m", [Condition (Expr M GreaterThan 2000) (Goto "hax"), Goto "hbx"])
    ]

tasks =
  Tasks
    2023
    19
    (CodeBlock 0)
    parser
    [ AssertExample "part 1 in result" (Right "qqz") $ \(w, p:_) ->
        accepts2 (mapLookupE "ae1" "in" w) p
    , task (uncurry part1) 19114 & taskPart 1
    , Assert "metaPartCount all" (4000 ^ 4) $ metaPartCount metaPartAll
    , Assert "metaAccepts empty" (4000 ^ 4)
        $ metaAccepts (Map.singleton "in" [Accept]) "in"
    , Assert "metaAccepts x > 2000" (4000 ^ 4 `div` 2)
        $ metaAccepts testWorkflows "hax"
    , Assert "metaAccepts x < 2001" (4000 ^ 4 `div` 2)
        $ metaAccepts testWorkflows "hax"
    , Assert "metaAccepts m then x" (4000 ^ 4 `div` 2)
        $ metaAccepts testWorkflows "h1m"
    , task (part2 . fst) 167409079868000 & taskPart 2
    ]
