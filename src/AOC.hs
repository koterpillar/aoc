{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AOC
  ( exampleY
  , example
  , inputY
  , input
  , InputSource(..)
  , readLines
  , readParse
  ) where

import           Control.Exception           (catch)

import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text

import           Data.Time                   (UTCTime (..), getCurrentTime,
                                              toGregorian)

import           GHC.IO.Exception            (IOException)

import           Network.HTTP.Client.Conduit (Request (..))
import           Network.HTTP.Simple

import           System.IO                   (isEOF)

import           Text.Parsec.Text            (Parser)

import           Utils

trim :: Text -> Text
trim = Text.dropWhile (== '\n') . Text.dropWhileEnd (== '\n')

catchIOException :: IO a -> IO a -> IO a
catchIOException action fallback = catch @IOException action $ const fallback

readSession :: IO (Maybe Text)
readSession =
  catchIOException (Just . trim <$> Text.readFile ".session-cookie") $
  pure Nothing

simpleRequest :: Text -> IO Text
simpleRequest url = do
  request <- parseRequestThrow $ Text.unpack url
  request' <-
    flip fmap readSession $ \case
      Nothing -> request
      Just session ->
        request
          {requestHeaders = [("Cookie", "session=" <> Text.encodeUtf8 session)]}
  response <- getResponseBody <$> httpBS request'
  pure $ Text.decodeUtf8 response

dropAfter :: Text -> Text -> Text
dropAfter marker contents =
  case Text.breakOn marker contents of
    (_, result)
      | Text.null result ->
        error $
        "Could not find marker: " <>
        Text.unpack marker <> " in contents: " <> Text.unpack contents
      | otherwise -> Text.drop (Text.length marker) result

dropBefore :: Text -> Text -> Text
dropBefore marker contents =
  case Text.breakOn marker contents of
    (result, rest)
      | Text.null rest ->
        error $
        "Could not find marker: " <>
        Text.unpack marker <> " in contents: " <> Text.unpack contents
      | otherwise -> result

exampleY :: Integer -> Int -> IO Text
exampleY year day =
  withCacheFile (".example-" <> tshow year <> "-" <> tshow day) $ do
    page <-
      simpleRequest $
      "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day
    let exampleBegin = "<pre><code>"
    let exampleEnd = "\n</code></pre>"
    pure $ dropBefore exampleEnd $ dropAfter exampleBegin page

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
  pure y

example :: Int -> IO Text
example day = do
  year <- currentYear
  exampleY year day

inputY :: Integer -> Int -> IO Text
inputY year day =
  withCacheFile (".input-" <> tshow year <> "-" <> tshow day) $
  simpleRequest $
  "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day <> "/input"

input :: Int -> IO Text
input day = do
  year <- currentYear
  inputY year day

withCacheFile :: Text -> IO Text -> IO Text
withCacheFile fileName action =
  let fileName' = Text.unpack fileName
   in catchIOException (Text.readFile fileName') $ do
        result <- action
        Text.writeFile fileName' result
        pure result

data InputSource
  = StandardInput
  | Example Int
  | Input Int
  deriving (Show)

readLines :: InputSource -> IO [Text]
readLines StandardInput =
  isEOF >>= \case
    True -> pure []
    False ->
      Text.getLine >>= \s ->
        case s of
          "" -> pure []
          _  -> fmap (s :) (readLines StandardInput)
readLines (Example day) = Text.lines <$> example day
readLines (Input day) = Text.lines <$> input day

readParse :: Parser a -> InputSource -> IO [a]
readParse parser source = map (justParse parser) <$> readLines source
