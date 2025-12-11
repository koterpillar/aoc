module Y2018.Day05 (
    tasks,
) where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import AOC
import Utils

parser :: Parser Text Text
parser = pureP ttrim

alpha :: [Char]
alpha = ['a' .. 'z']

reducePolymer :: Text -> Text
reducePolymer = foldr ((.) . go) id alpha
  where
    go c = Text.replace (mk2 c (toUpper c)) "" . Text.replace (mk2 (toUpper c) c) ""
      where
        mk2 c1 c2 = Text.pack [c1, c2]

part1 :: Text -> Int
part1 = Text.length . iterateSettleL reducePolymer

part2 :: Text -> Int
part2 p = minimum $ map (\c -> part1 $ removeChar c p) alpha
  where
    removeChar c = Text.filter (\x -> toLower x /= c)

tasks =
    Tasks
        (AOC 2018 5)
        (InlineCode 0)
        parser
        [ task part1 10 & taskPart 1
        , task part2 4 & taskPart 2
        ]
