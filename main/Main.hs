import qualified Data.Map           as Map
import qualified Data.Text          as Text

import           System.Environment (getArgs)

import           AOC
import           AllTasks
import           Utils

data Selector
  = All
  | Last
  | LastInYear Integer
  | Year Integer
  | One Integer Int
  deriving (Show)

plast :: [a] -> [a]
plast = pure . last

filterYear :: Integer -> [Tasks] -> [Tasks]
filterYear y = filter $ \(Tasks ty _ _ _) -> ty == y

selectTasks :: Selector -> [Tasks] -> [Tasks]
selectTasks All            = id
selectTasks Last           = plast
selectTasks (LastInYear y) = plast . filterYear y
selectTasks (Year y)       = filterYear y
selectTasks (One y d)      = filter $ \(Tasks ty td _ _) -> ty == y && td == d

parseSelector :: [String] -> Selector
parseSelector []          = Last
parseSelector ["all"]     = All
parseSelector [ys]        = LastInYear (read ys)
parseSelector [ys, "all"] = Year (read ys)
parseSelector [ys, ds]    = One (read ys) (read ds)
parseSelector _           = error "invalid selector"

main = do
  selector <- parseSelector <$> getArgs
  traverse_ processTasks $ selectTasks selector allTasks
