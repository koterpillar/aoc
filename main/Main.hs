import qualified Data.Map           as Map
import qualified Data.Text          as Text

import           System.Environment (getArgs)

import           AllTasks
import           AOC
import           Utils

data Selector
  = All
  | Last
  | Year Integer
  | One Integer Int
  deriving (Show)

selectTasks :: Selector -> [Tasks] -> [Tasks]
selectTasks All ts       = ts
selectTasks Last ts      = [last ts]
selectTasks (Year y) ts  = [t | t@(Tasks ty _ _ _) <- ts, ty == y]
selectTasks (One y d) ts = [t | t@(Tasks ty td _ _) <- ts, ty == y, td == d]

parseSelector :: [String] -> Selector
parseSelector []       = Last
parseSelector ["all"]  = All
parseSelector [ys]     = Year (read ys)
parseSelector [ys, ds] = One (read ys) (read ds)
parseSelector _        = error "invalid selector"

main = do
  selector <- parseSelector <$> getArgs
  traverse_ processTasks $ selectTasks selector allTasks
