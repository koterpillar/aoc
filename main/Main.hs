import qualified Data.Map as Map
import qualified Data.Text as Text

import System.Environment (getArgs)

import AOC
import AllTasks
import Utils

data Selector
    = All
    | Last
    | LastInYear Integer
    | Year Integer
    | One Integer Int
    deriving (Show)

filterYear :: Integer -> [Tasks] -> [Tasks]
filterYear y = filter $ \(Tasks (AOC ty _) _ _ _) -> ty == y

maybeLast :: [a] -> [a]
maybeLast = toList . fmap snd . unsnoc

selectTasks :: Selector -> [Tasks] -> [Tasks]
selectTasks All = id
selectTasks Last = maybeLast
selectTasks (LastInYear y) = maybeLast . filterYear y
selectTasks (Year y) = filterYear y
selectTasks (One y d) = filter $ \(Tasks (AOC ty td) _ _ _) -> ty == y && td == d

parseSelector :: [String] -> Selector
parseSelector [] = Last
parseSelector ["all"] = All
parseSelector [ys] = LastInYear (read ys)
parseSelector [ys, "all"] = Year (read ys)
parseSelector [ys, ds] = One (read ys) (read ds)
parseSelector _ = error "invalid selector"

main = do
    selector <- parseSelector <$> getArgs
    traverse_ processTasks $ selectTasks selector allTasks
