module Y2022.Day24 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser :: Parser Text ()
parser = error "parser"

part1 = error . show

tasks =
  Tasks
    2022
    24
    ShowExamples
    parser
    [ Task part1 ()
    ]
