module Utils
  ( module Utils
  , module Data.Containers.ListUtils
  , module Data.Either
  , module Data.Either.Extra
  , module Data.Function.Memoize
  , module Data.Maybe
  , module Data.List
  , module Data.Tuple
  , module Utils.Base
  , module Utils.Iterate
  , module Utils.Map
  , module Utils.Set
  , module Utils.Text
  , module Utils.Trace
  , Map
  , Set
  , Text
  , ($>)
  , (&)
  , (<=<)
  , (<|>)
  , (>=>)
  , bimap
  , chr
  , chunksOf
  , enumerate
  , first
  , for
  , for_
  , guard
  , isDigit
  , isLower
  , isUpper
  , join
  , liftA2
  , maximumOn
  , minimumOn
  , on
  , ord
  , readEither
  , replicateM
  , replicateM_
  , second
  , splitOn
  , toList
  , traverse_
  , unless
  , void
  , when
  ) where

import           Control.Applicative       (liftA2, (<|>))
import           Control.Monad

import           Data.Bifunctor            (bimap, first, second)

import           Data.Char                 (chr, isDigit, isLower, isUpper, ord)

import           Data.Containers.ListUtils (nubInt, nubOrd)

import           Data.Either
import           Data.Either.Extra

import           Data.Foldable             (for_, toList, traverse_)

import           Data.Functor              (($>))

import           Data.Function             (on, (&))
import           Data.Function.Memoize

import           Data.List
import           Data.List.Extra

import           Data.Map                  (Map)

import           Data.Maybe

import           Data.Set                  (Set)

import           Data.Text                 (Text)

import           Data.Traversable          (for)

import           Data.Tuple

import           Text.Read

import           Utils.Base
import           Utils.Iterate
import           Utils.Map
import           Utils.Set
import           Utils.Text
import           Utils.Trace

mostCommon :: Ord a => [a] -> Maybe a
mostCommon = fmap snd . maybeMaximum . map swap . mapToList . mapFromListCount
