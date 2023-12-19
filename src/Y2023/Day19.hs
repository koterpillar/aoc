module Y2023.Day19 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Category
  = X
  | M
  | A
  | S
  deriving (Ord, Eq, Show, Bounded, Enum)

data Sign
  = LessThan
  | GreaterThan
  deriving (Ord, Eq, Show, Bounded, Enum)

data Workflow
  = Condition Category Sign Int Workflow
  | Goto Text
  | Accept
  | Reject
  deriving (Ord, Eq, Show)

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

mkCondition :: (Category, (Sign, Int)) -> Workflow -> Workflow
mkCondition (c, (s, i)) = Condition c s i

exprP :: Parser Text (Category, (Sign, Int))
exprP =
  charactersP
    &* (unconsP
          &* (categoryP
                &= (unconsP &* (choiceEBP "<>" &= (pureP Text.pack &* integerP)))))

workflowP :: Parser Text Workflow
workflowP =
  (Accept <$ requireP "A")
    &| (Reject <$ requireP "R")
    &| (tsplitP ":" &* ap2P mkCondition exprP workflowP)
    &| (Goto <$> idP)

workflowsP :: Parser [Text] Workflows
workflowsP =
  traverseP
    (tsplitP "{"
       &* (idP &+ (pureP (Text.dropEnd 1) &* (tsplitP "," &** workflowP))))
    &* pureP Map.fromList

parser :: Parser Text (Workflows, [Part])
parser = lineGroupsP &* (workflowsP &+ traverseP partP)

accepts1 :: Workflow -> Part -> Maybe (Either Bool Text)
accepts1 Accept _ = Just $ Left True
accepts1 Reject _ = Just $ Left False
accepts1 (Goto x) _ = Just $ Right x
accepts1 (Condition c s v w) p =
  if rating c p `op` v
    then accepts1 w p
    else Nothing
  where
    op =
      case s of
        LessThan    -> (<)
        GreaterThan -> (>)

accepts2 :: [Workflow] -> Part -> Either Bool Text
accepts2 [] p = error $ "Part " <> show p <> " not accepted by any workflow"
accepts2 (w:ws) p =
  case accepts1 w p of
    Just r  -> r
    Nothing -> accepts2 ws p

accepts :: Workflows -> Text -> Part -> Bool
accepts w i p =
  case accepts2 ws p of
    Left r   -> r
    Right i' -> accepts w i' p
  where
    ws = Map.findWithDefault [] i w

part1 :: (Workflows, [Part]) -> Int
part1 (w, ps) = sum $ map totalRating accepted
  where
    accepted = filter (accepts w "in") ps

tasks =
  Tasks
    2023
    19
    (CodeBlock 0)
    parser
    [ AssertExample "part 1 in result" (Right "qqz") $ \(w, p:_) ->
        accepts2 (mapLookupE "ae1" "in" w) p
    , task part1 19114 & taskPart 1
    ]
