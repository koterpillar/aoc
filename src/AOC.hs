{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AOC
  ( getExampleY
  , getExample
  , getInputY
  , getInput
  , processEI
  ) where

import           Control.Exception           (catch)
import           Control.Monad               (when)

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

getExampleY :: Integer -> Int -> IO Text
getExampleY year day =
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

getExample :: Int -> IO Text
getExample day = do
  year <- currentYear
  getExampleY year day

getInputY :: Integer -> Int -> IO Text
getInputY year day =
  withCacheFile (".input-" <> tshow year <> "-" <> tshow day) $
  simpleRequest $
  "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day <> "/input"

getInput :: Int -> IO Text
getInput day = do
  year <- currentYear
  getInputY year day

withCacheFile :: Text -> IO Text -> IO Text
withCacheFile fileName action =
  let fileName' = Text.unpack fileName
   in catchIOException (Text.readFile fileName') $ do
        result <- action
        Text.writeFile fileName' result
        pure result

processEI :: (Eq b, Show b) => Int -> (Text -> a) -> (a -> b) -> b -> IO ()
processEI day parse solve exampleExpected = do
  example <- parse <$> getExample day
  let exampleResult = solve example
  when (exampleResult /= exampleExpected) $
    error $
    "Example result is " <>
    show exampleResult <> ", but expected " <> show exampleExpected
  input <- parse <$> getInput day
  let result = solve input
  print result