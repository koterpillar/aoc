module Y2021.Day14 where

import qualified Data.Text as Text

import           AOC
import           Utils

type Element = Char

nullElement :: Element
nullElement = ' '

type Polymer = [Element]

type Pair = (Element, Element)

type Insertions = Map Pair Element

parse :: Parser Text (Polymer, Insertions)
parse =
  linesP &* unconsP &*
  (charactersP &= (mapFromList <$> pureP tail &* traverseP parseInsertion))
  where
    parseInsertion = tsplitP " -> " &* (charactersP &* pairP &+ charP)

type PolymerMap = Map Pair Int

toPairs :: Polymer -> PolymerMap
toPairs polymer =
  mapFromListCount $ zipTail (nullElement : polymer ++ [nullElement])

step :: Insertions -> PolymerMap -> PolymerMap
step instructions = mapFromListSum . concatMap go . mapToList
  where
    go (pair, count) = [(pair', count) | pair' <- go' pair]
    go' pair@(e1, e2) =
      case mapLookup pair instructions of
        Nothing -> [pair]
        Just e  -> [(e1, e), (e, e2)]

countMap :: PolymerMap -> Map Char Int
countMap =
  fmap (`div` 2) .
  mapFromListSum .
  filter (\(e, _) -> e /= nullElement) .
  concatMap (\((e1, e2), c) -> [(e1, c), (e2, c)]) . mapToList

solve steps (polymer, insertions) =
  diff $ iterateN steps (step insertions) (toPairs polymer)
  where
    diff = (\counts -> last counts - head counts) . sort . toList . countMap

part1 = solve 10

part2 = solve 40

tasks =
  Tasks
    2021
    14
    (CodeBlock 0)
    parse
    [ AssertExample "step" (toPairs "NCNBCHB") (\(p, i) -> step i $ toPairs p)
    , AssertExample
        "step"
        (toPairs "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
        (\(p, i) -> iterateN 4 (step i) $ toPairs p)
    , Assert
        "countMap"
        (mapFromList [('A', 2), ('B', 2)])
        (countMap $ toPairs "ABAB")
    , Task part1 1588
    , Task part2 2188189693529
    ]
