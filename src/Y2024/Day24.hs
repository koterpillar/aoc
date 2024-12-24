module Y2024.Day24
  ( tasks
  ) where

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
    2024
    24
    ShowExamples
    parser
    [ task part1 () & taskPart 1
    ]
