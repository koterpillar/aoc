import           Data.List
import           Data.List.Split
import           Data.List.Utils

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Maybe

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Utils

type Name = String

type Weight = Int

data Program =
  Program
    { pName     :: Name
    , pWeight   :: Weight
    , pChildren :: [Name]
    }
  deriving (Eq, Show)

parseLine :: String -> Program
parseLine ln =
  case words ln of
    [name, wstr] -> Program name (parseWeight wstr) []
    (name:wstr:_:children) ->
      Program name (parseWeight wstr) $ map (replace "," "") children

parseWeight :: String -> Weight
parseWeight = read . drop 1 . reverse . drop 1 . reverse

readPrograms :: IO [Program]
readPrograms = map parseLine <$> readLines

roots :: [Program] -> [Name]
roots = roots' Set.empty Set.empty
  where
    roots' candidates rejected [] =
      Set.toList $ candidates `Set.difference` rejected
    roots' candidates rejected (Program name _ children:rest) =
      roots' candidates' rejected' rest
      where
        candidates' = Set.insert name $ candidates `Set.difference` children'
        rejected' = rejected `Set.union` children'
        children' = Set.fromList children

type ProgramTree = Map Name Program

makeTree :: [Program] -> ProgramTree
makeTree = Map.fromList . map (\p -> (pName p, p))

type Result = (Name, Weight) -- weight difference

totalWeight :: ProgramTree -> Name -> Either Result Weight
totalWeight pt name = do
  let (Just p) = Map.lookup name pt
  childWeights <- traverse (totalWeight pt) (pChildren p)
  case discrepancy childWeights of
    Nothing -> pure $ pWeight p + sum childWeights
    Just (right, wrong) -> do
      let ((offending, _):_) =
            filter (\(_, w) -> w == wrong) (zip (pChildren p) childWeights)
      Left (offending, wrong - right)

discrepancy :: Ord a => [a] -> Maybe (a, a) -- right and wrong
discrepancy [] = Nothing
discrepancy [_] = Nothing
discrepancy [_, _] = Nothing
discrepancy (x1:x2:x3:xs)
  | x1 == x2 =
    case filter (/= x1) (x3 : xs) of
      []      -> Nothing
      [wrong] -> Just (x1, wrong)
      _       -> error "too many different values"
  | x1 == x3 = Just (x1, x2)
  | x2 == x3 = Just (x2, x1)
  | otherwise = error "too many different values"
