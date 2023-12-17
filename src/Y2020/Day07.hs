module Y2020.Day07 where

import qualified Data.Text as Text

import           AOC
import           Graph
import           Memo
import           Utils

type Bag = Text

parseBag :: Parser Text Bag
parseBag = pureP $ terase " bag" . terase " bags"

parseInner :: Parser Text (Int, Bag)
parseInner = tspanP isDigit &* integerP &= (pureP Text.tail &* parseBag)

type Rule = (Bag, Set (Int, Bag))

parseInners :: Parser Text (Set (Int, Bag))
parseInners =
  setFromList <$>
  pureP (terase ".") &* tsplitP ", " &* pureP (filter (/= "no other bags")) &**
  parseInner

parseRule :: Parser Text Rule
parseRule = tsplitP " contain " &* parseBag &+ parseInners

type Rules = Map Bag (Set (Int, Bag))

parseRules :: Parser Text Rules
parseRules = mapFromList <$> linesP &** parseRule

mkGraph :: Rules -> Graph Bag
mkGraph = fmap $ setMap snd

shinyGold :: Bag
shinyGold = "shiny gold"

findContainers :: Rules -> Int
findContainers =
  pred . length . reachableFrom shinyGold . reverseGraph . mkGraph

findCost :: Rules -> Int
findCost rules = pred $ findCost' rules shinyGold

findCost' :: Rules -> Bag -> Int
findCost' rules = unsafeMemo go
  where
    go bag =
      let inners = mapLookupE "rule" bag rules
       in 1 + sum [n * go inner | (n, inner) <- toList inners]

tasks =
  Tasks
    2020
    7
    (CodeBlock 0)
    parseRules
    [Task findContainers 4, Task findCost 32]
