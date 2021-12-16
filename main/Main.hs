import qualified Data.Map           as Map
import qualified Data.Text          as Text

import           System.Environment (getArgs)

import           AOC
import           Utils

import qualified Y2021.Day01
import qualified Y2021.Day02
import qualified Y2021.Day03
import qualified Y2021.Day04
import qualified Y2021.Day05
import qualified Y2021.Day06
import qualified Y2021.Day07
import qualified Y2021.Day08
import qualified Y2021.Day09
import qualified Y2021.Day10
import qualified Y2021.Day11
import qualified Y2021.Day12
import qualified Y2021.Day13
import qualified Y2021.Day14
import qualified Y2021.Day15
import qualified Y2021.Day16

allTasks :: [Tasks]
allTasks =
  [ Y2021.Day01.tasks
  , Y2021.Day02.tasks
  , Y2021.Day03.tasks
  , Y2021.Day04.tasks
  , Y2021.Day05.tasks
  , Y2021.Day06.tasks
  , Y2021.Day07.tasks
  , Y2021.Day08.tasks
  , Y2021.Day09.tasks
  , Y2021.Day10.tasks
  , Y2021.Day11.tasks
  , Y2021.Day12.tasks
  , Y2021.Day13.tasks
  , Y2021.Day14.tasks
  , Y2021.Day15.tasks
  , Y2021.Day16.tasks
  ]

type TasksKey = (Integer, Int)

taskKey :: Tasks -> TasksKey
taskKey (Tasks y d _ _) = (y, d)

data Selector
  = All
  | Last
  | One TasksKey
  deriving (Show)

selectTasks :: Selector -> [Tasks] -> [Tasks]
selectTasks All ts     = ts
selectTasks Last ts    = [last ts]
selectTasks (One k) ts = [t | t <- ts, taskKey t == k]

parseSelector :: [String] -> Selector
parseSelector []       = Last
parseSelector ["all"]  = All
parseSelector [ys, ds] = One (read ys, read ds)
parseSelector _        = error "invalid selector"

main = do
  selector <- parseSelector <$> getArgs
  traverse_ processTasks $ selectTasks selector allTasks
