module Counts where

import qualified Data.Map as Map
import           Utils

newtype Counts a =
  Counts
    { getCounts :: Map a Int
    }
  deriving (Eq, Ord, Show)

cpure :: Ord a => a -> Counts a
cpure x = Counts $ Map.singleton x 1

cFromListCount :: Ord a => [(a, Int)] -> Counts a
cFromListCount = Counts . mapFromListSum

cFromList :: Ord a => [a] -> Counts a
cFromList = cFromListCount . flip zip (repeat 1)

cToList :: Counts a -> [(a, Int)]
cToList = Map.toList . getCounts

cToSingle :: Show a => Counts a -> a
cToSingle c =
  case cToList c of
    [(x, 1)] -> x
    vs       -> error $ "cToSingle: " ++ show vs

ctotal :: Counts a -> Int
ctotal = sum . Map.elems . getCounts

cmap :: Ord b => (a -> b) -> Counts a -> Counts b
cmap f = Counts . Map.mapKeysWith (+) f . getCounts

cmapf :: Ord b => Counts a -> (a -> b) -> Counts b
cmapf = flip cmap

cjoin :: Ord a => Counts (Counts a) -> Counts a
cjoin = Counts . mapFromListSum . concatMap toListMul . cToList
  where
    toListMul (c, n) = [(v, m * n) | (v, m) <- cToList c]

cflatMap :: Ord b => (a -> Counts b) -> Counts a -> Counts b
cflatMap f = cjoin . cmap f

call :: (a -> Bool) -> Counts a -> Bool
call f = all f . Map.keys . getCounts

crights :: Ord b => Counts (Either a b) -> Maybe (Counts b)
crights = fmap cFromListCount . traverse go . cToList
  where
    go (Right b, c) = Just (b, c)
    go _            = Nothing

cexamples :: Show a => Counts a -> String
cexamples gs =
  "Total: " <>
  show (ctotal gs) <>
  " Examples: " <> intercalate ", " (map show $ take 2 $ cToList gs)

traceCounts :: Show a => Counts a -> Counts a
traceCounts = traceF cexamples
