import Data.List
import Data.List.Split
import Data.List.Utils

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

data Program = Program
  { pName :: String
  , pChildren :: [String]
  }
  deriving (Eq, Show)

parseLine :: String -> Program
parseLine ln =
  case words ln of
    [name, _] -> Program name []
    (name:_:_:children) -> Program name $ map (replace "," "") children

parseTree :: IO [Program]
parseTree = map parseLine <$> readLines

roots :: [Program] -> [String]
roots = roots' Set.empty Set.empty
  where
    roots' candidates rejected [] = Set.toList $ candidates `Set.difference` rejected
    roots' candidates rejected (Program name children:rest) = roots' candidates' rejected' rest
      where
        candidates' = Set.insert name $ candidates `Set.difference` children'
        rejected' = rejected `Set.union` children'
        children' = Set.fromList children
