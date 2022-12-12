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
  , module Utils.Map
  , module Utils.Set
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
  , guard
  , first
  , for
  , for_
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
import qualified Data.Map                  as Map

import           Data.Maybe

import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Data.Traversable          (for)

import           Data.Tuple

import           Text.Read

import           Utils.Base
import           Utils.Map
import           Utils.Set
import           Utils.Trace

mostCommon :: Ord a => [a] -> Maybe a
mostCommon = fmap snd . maybeMaximum . map swap . Map.toList . mapFromListCount
