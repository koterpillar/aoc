{-# LANGUAGE OverloadedStrings #-}

module AOC where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Time           (UTCTime (..), getCurrentTime, toGregorian)
import           Network.HTTP.Simple

import qualified Data.Text.IO        as Text
import           Utils

simpleRequest :: Text -> IO Text
simpleRequest url = do
  address <- parseRequestThrow $ Text.unpack url
  response <- getResponseBody <$> httpBS address
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
exampleY year day = do
  page <-
    simpleRequest $
    "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day
  let exampleBegin = "<pre><code>"
  let exampleEnd = "\n</code></pre>"
  pure $ dropBefore exampleEnd $ dropAfter exampleBegin page

example :: Int -> IO Text
example day = do
  currentDay <- utctDay <$> getCurrentTime
  let (currentYear, _, _) = toGregorian currentDay
  exampleY currentYear day
