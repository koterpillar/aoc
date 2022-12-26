module Utils.Text where

import           Data.Function.Memoize

import           Data.Text             (Text)
import qualified Data.Text             as Text

import           GHC.Stack             (HasCallStack)

pad :: Int -> Text -> Text
pad sz = Text.justifyRight sz ' '

ttrim :: Text -> Text
ttrim = Text.dropWhile (== '\n') . Text.dropWhileEnd (== '\n')

tshow :: Show a => a -> Text
tshow = Text.pack . show

terror :: HasCallStack => Text -> c
terror = error . Text.unpack

treplace :: Text -> Text -> Text -> Text
treplace = Text.replace

terase :: Text -> Text -> Text
terase piece = Text.replace piece ""

tlength :: Text -> Int
tlength = Text.length

instance Memoizable Text where
  memoize f t = memoize (f . Text.pack) (Text.unpack t)
