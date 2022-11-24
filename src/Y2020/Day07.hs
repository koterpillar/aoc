module Y2020.Day07 where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Graph
import           Utils

type Bag = Text

parseBag :: Parser Text Bag
parseBag = pureP $ Text.replace " bag" "" . Text.replace " bags" ""

parseInner :: Parser Text (Int, Bag)
parseInner = tspanP isDigit &* (integerP &= pureP Text.tail &* parseBag)

type Rule = (Bag, Set (Int, Bag))

parseInners :: Parser Text (Set (Int, Bag))
parseInners =
  setFromList <$>
  pureP (Text.replace "." "") &* tsplitP ", " &*
  pureP (filter (/= "no other bags")) &**
  parseInner

parseRule :: Parser Text Rule
parseRule = tsplitP " contain " &* (parseBag &+ parseInners)

type Rules = Map Bag (Set (Int, Bag))

parseRules :: Parser Text Rules
parseRules = mapFromList <$> linesP &** parseRule

mkGraph :: Rules -> Graph Bag
mkGraph = fmap $ Set.map snd

shinyGold :: Bag
shinyGold = "shiny gold"

findContainers :: Rules -> Int
findContainers =
  pred . length . reachableFrom shinyGold . reverseGraph . mkGraph

findCost :: Rules -> Int
findCost rules = pred $ findCost' rules shinyGold

findCost' :: Rules -> Bag -> Int
findCost' rules =
  memoFix $ \memoFind bag ->
    case mapLookup bag rules of
      Nothing     -> error $ "no rule for " ++ show bag
      Just inners -> 1 + sum [n * memoFind inner | (n, inner) <- toList inners]

tasks =
  Tasks
    2020
    7
    (CodeBlock 0)
    parseRules
    [Task findContainers 4, Task findCost 32]
