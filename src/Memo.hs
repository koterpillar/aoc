module Memo
  ( Hashable(..)
  , unsafeMemo
  , unsafeMemo2
  , unsafeMemo3
  ) where

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HashMap

import           Data.IORef

import           Data.Tuple.Extra    (curry3, uncurry3)

import           GHC.IO

import           Utils

unsafeMemo :: (Hashable a, Show a, Show o) => (a -> o) -> (a -> o)
unsafeMemo fn =
  let c = unsafePerformIO $ newIORef HashMap.empty
   in \a ->
        unsafePerformIO $ do
          m <- readIORef c
          case HashMap.lookup a m of
            Just o -> pure o
            Nothing -> do
              let o = fn a
              modifyIORef' c $ HashMap.insert a o
              pure o

unsafeMemo2 ::
     (Hashable a, Hashable b, Show a, Show b, Show o)
  => (a -> b -> o)
  -> (a -> b -> o)
unsafeMemo2 = curry . unsafeMemo . uncurry

unsafeMemo3 ::
     (Hashable a, Hashable b, Hashable c, Show a, Show b, Show c, Show o)
  => (a -> b -> c -> o)
  -> (a -> b -> c -> o)
unsafeMemo3 = curry3 . unsafeMemo . uncurry3
