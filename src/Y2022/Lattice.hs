{-# LANGUAGE FlexibleContexts #-}

module Y2022.Lattice
  ( Lattice
  , latticeEmpty
  , latticeInsert
  , latticeInsertS
  , Positionable(..)
  ) where

import           Control.Monad.State.Class

import qualified Data.Map                  as Map

import           Utils

data Lattice
  = LEmpty
  | LTree L
  deriving (Show)

data L
  = LLeaf
  | LChoice (Map Int L)
  deriving (Show)

class Positionable a where
  latticePosition :: a -> [Int]

latticeEmpty :: Lattice
latticeEmpty = LEmpty

latticeInsert :: Positionable a => Lattice -> a -> Maybe Lattice
latticeInsert LEmpty    = Just . LTree . mkSingleton . latticePosition
latticeInsert (LTree l) = fmap LTree . (`lInsert` l) . latticePosition

latticeInsertS ::
     MonadState Lattice s
  => Positionable a =>
       a -> s Bool
latticeInsertS a =
  gets (`latticeInsert` a) >>= \case
    Nothing -> pure False
    Just l  -> put l $> True

mkSingleton :: [Int] -> L
mkSingleton = foldr ((LChoice .) . Map.singleton) LLeaf

lInsert :: [Int] -> L -> Maybe L
lInsert _ LLeaf = Nothing
lInsert [] l = error $ "lInsert: no more elements, still have: " ++ show l
lInsert (x:xs) (LChoice m)
  | superseded = Nothing
  | otherwise =
    case Map.lookup x m of
      Nothing -> Just $ LChoice $ insertHere $ mkSingleton xs
      Just m' -> LChoice . insertHere <$> lInsert xs m'
  where
    candidates = [v | (k, v) <- Map.toList m, k > x]
    superseded = any (isNothing . lInsert xs) candidates
    insertHere v = Map.insert x v m
