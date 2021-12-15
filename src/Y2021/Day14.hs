module Y2021.Day14 where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import           AOC
import           Utils

type Element = Char

nullElement :: Element
nullElement = ' '

type Polymer = [Element]

type Pair = (Element, Element)

type Insertions = Map Pair Element

parse :: Text -> (Polymer, Insertions)
parse = go . Text.lines
  where
    go (polymer:_:insertions) =
      (Text.unpack polymer, parseInsertions insertions)
    go invalid = error $ "Invalid input: " <> show invalid
    parseInsertions = Map.fromList . map parseInsertion
    parseInsertion ln =
      case Text.splitOn " -> " ln of
        [from, to] ->
          case (Text.unpack from, Text.unpack to) of
            ([c1, c2], [c3]) -> ((c1, c2), c3)
            other            -> error $ "Invalid insertion: " <> show other
        other -> error $ "Invalid insertion: " <> show other

type PolymerMap = Map Pair Int

toPairs :: Polymer -> PolymerMap
toPairs polymer =
  mapFromListSum $
  zipWith
    (\e1 e2 -> ((e1, e2), 1))
    (nullElement : polymer)
    (polymer ++ [nullElement])

step :: Insertions -> PolymerMap -> PolymerMap
step instructions = mapFromListSum . concatMap go . Map.toList
  where
    go (pair, count) = [(pair', count) | pair' <- go' pair]
    go' pair@(e1, e2) =
      case Map.lookup pair instructions of
        Nothing -> [pair]
        Just e  -> [(e1, e), (e, e2)]

countMap :: PolymerMap -> Map Char Int
countMap =
  Map.map (`div` 2) .
  mapFromListSum .
  filter (\(e, _) -> e /= nullElement) .
  concatMap (\((e1, e2), c) -> [(e1, c), (e2, c)]) . Map.toList

solve steps (polymer, insertions) =
  diff $ iterateN steps (step insertions) (toPairs polymer)
  where
    diff = (\counts -> last counts - head counts) . sort . Map.elems . countMap

part1 = solve 10

part2 = solve 40

tasks =
  Tasks
    2021
    14
    parse
    [ AssertExample "step" (toPairs "NCNBCHB") (\(p, i) -> step i $ toPairs p)
    , AssertExample
        "step"
        (toPairs "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
        (\(p, i) -> iterateN 4 (step i) $ toPairs p)
    , Assert
        "countMap"
        (Map.fromList [('A', 2), ('B', 2)])
        (countMap $ toPairs "ABAB")
    , Task part1 1588
    , Task part2 2188189693529
    ]
