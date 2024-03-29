module Utils.Trace
  ( module Utils.Trace
  , trace
  , traceM
  , traceShow
  , traceShowId
  , traceShowM
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Debug.Trace
import           GHC.IO
import           System.IO

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

{-# NOINLINE btrace #-}
btrace :: ByteString -> a -> a
btrace m a =
  unsafePerformIO $ do
    ByteString.hPutStr stdout m
    pure a

btraceF :: (a -> ByteString) -> a -> a
btraceF f a = btrace (f a) a

{-# NOINLINE lbtrace #-}
lbtrace :: LBS.ByteString -> a -> a
lbtrace m a =
  unsafePerformIO $ do
    LBS.hPutStr stdout m
    pure a

lbtraceF :: (a -> LBS.ByteString) -> a -> a
lbtraceF f a = lbtrace (f a) a

prependShow :: Show a => String -> a -> String
prependShow msg a = msg ++ " " ++ show a

traceO :: Show o => String -> o -> o
traceO msg = traceF (prependShow $ msg ++ ">")

traceAO :: (Show a, Show o) => String -> (a -> o) -> a -> o
traceAO msg fn = traceO msg . fn . traceF (prependShow $ msg ++ "<")

traceAO2 ::
     (Show a1, Show a2, Show o) => String -> (a1 -> a2 -> o) -> a1 -> a2 -> o
traceAO2 msg fn =
  curry
    $ traceO msg
        . uncurry fn
        . traceF (\(a1, a2) -> msg ++ "< " ++ show a1 ++ show a2)
