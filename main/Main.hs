{-# OPTIONS_GHC -F -pgmF./enumerate_days #-}

import qualified Data.Map           as Map
import qualified Data.Text          as Text

import           System.Environment (getArgs)

import           AOC
import           Utils

-- IMPORTS

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

-- ALL TASKS
