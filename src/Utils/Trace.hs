module Utils.Trace
  ( module Utils.Trace
  , traceM
  , traceShow
  , traceShowId
  , traceShowM
  ) where

import           Data.Text   (Text)
import qualified Data.Text   as Text

import           Debug.Trace

traceShowF :: Show b => (a -> b) -> a -> a
traceShowF f a = traceShow (f a) a

ttrace :: Text -> a -> a
ttrace = trace . Text.unpack

ttraceF :: (a -> Text) -> a -> a
ttraceF f a = ttrace (f a) a
