module Utils
  ( module Utils
  , module Data.Containers.ListUtils
  , module Data.Either
  , module Data.Either.Extra
  , module Data.Maybe
  , module Data.List
  , module Data.List.Extra
  , module Data.Tuple
  , module Lens.Micro.Platform
  , module Utils.Base
  , module Utils.Iterate
  , module Utils.Map
  , module Utils.Set
  , module Utils.Text
  , module Utils.Trace
  , Map
  , Set
  , Text
  , (.:)
  , (.:.)
  , ($>)
  , (<=<)
  , (<|>)
  , (>=>)
  , (&&&)
  , (***)
  , bimap
  , chr
  , first
  , foldlM
  , foldrM
  , for
  , for_
  , guard
  , isDigit
  , isLower
  , isUpper
  , join
  , liftA2
  , on
  , ord
  , readEither
  , replicateM
  , replicateM_
  , second
  , toList
  , traverse_
  , unless
  , void
  , when
  , whileM
  ) where

import           Control.Applicative       (liftA2, (<|>))
import           Control.Arrow             ((&&&), (***))
import           Control.Monad
import           Control.Monad.Extra

import           Data.Bifunctor            (bimap, first, second)

import           Data.Char                 (chr, isDigit, isLower, isUpper, ord)

import           Data.Composition          ((.:), (.:.))

import           Data.Containers.ListUtils (nubInt)

import           Data.Either
import           Data.Either.Extra

import           Data.Foldable             (foldlM, foldrM, for_, toList,
                                            traverse_)

import           Data.Functor              (($>))

import           Data.Function             (on)

import           Data.List
import           Data.List.Extra

import           Data.Map                  (Map)

import           Data.Maybe

import           Data.Set                  (Set)

import           Data.Text                 (Text)

import           Data.Traversable          (for)

import           Data.Tuple

import           Lens.Micro.Platform

import           Text.Read

import           Utils.Base
import           Utils.Iterate
import           Utils.Map
import           Utils.Set
import           Utils.Text
import           Utils.Trace

mostCommon :: Ord a => [a] -> Maybe a
mostCommon = fmap snd . maybeMaximum . map swap . mapToList . mapFromListCount
