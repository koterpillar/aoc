module Utils.Trace
  ( module Utils.Trace
  , trace
  , traceM
  , traceShow
  , traceShowId
  , traceShowM
  ) where

import           Data.Text   (Text)
import qualified Data.Text   as Text

import           Debug.Trace

traceF :: (a -> String) -> a -> a
traceF f a = trace (f a) a

traceShowF :: Show b => (a -> b) -> a -> a
traceShowF f a = traceShow (f a) a

ttrace :: Text -> a -> a
ttrace = trace . Text.unpack

ttraceF :: (a -> Text) -> a -> a
ttraceF f a = ttrace (f a) a

ttraceM :: Monad m => Text -> m ()
ttraceM = traceM . Text.unpack

prependShow :: Show a => String -> a -> String
prependShow msg a = msg ++ " " ++ show a

traceO :: Show o => String -> o -> o
traceO msg = traceF (prependShow $ msg ++ ">")

traceAO :: (Show a, Show o) => String -> (a -> o) -> a -> o
traceAO msg fn = traceO msg . fn . traceF (prependShow $ msg ++ "<")

traceAO2 ::
     (Show a1, Show a2, Show o) => String -> (a1 -> a2 -> o) -> a1 -> a2 -> o
traceAO2 msg fn =
  curry $
  traceO msg .
  uncurry fn . traceF (\(a1, a2) -> msg ++ "< " ++ show a1 ++ show a2)
