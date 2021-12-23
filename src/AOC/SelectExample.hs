module AOC.SelectExample where

import qualified Data.Text as Text

import           Utils

selectExample :: Integer -> Int -> [Text] -> Text
selectExample _ _ [example] = example
selectExample 2021 1 examples = head examples
selectExample 2021 4 examples = head examples
selectExample 2021 5 examples = head examples
selectExample 2021 6 examples = head examples
selectExample 2021 8 examples = examples !! 2
selectExample 2021 9 examples = head examples
selectExample 2021 10 examples = last examples
selectExample 2021 11 examples = head examples
selectExample 2021 12 examples = head examples
selectExample 2021 13 examples = examples !! 1
selectExample 2021 14 examples = head examples
selectExample 2021 15 examples = head examples
selectExample 2021 16 _ = "A0016C880162017C3686B18A3D4780"
selectExample 2021 17 examples = head examples
selectExample 2021 18 examples = examples !! 7
selectExample 2021 19 examples = examples !! 5
selectExample 2021 20 examples = head examples
selectExample 2021 22 examples = examples !! 2
selectExample year day examples =
  error $
  "Cannot select from " <>
  show (length examples) <>
  " examples for year " <>
  show year <>
  " day " <>
  show day <>
  ":\n\n" <>
  Text.unpack
    (Text.unlines $
     zipWith (\i e -> "--- item " <> tshow i <> " ---\n" <> e) [0 ..] examples)
