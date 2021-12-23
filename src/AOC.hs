{-# LANGUAGE GADTs #-}

module AOC
  ( getExample
  , getInput
  , Tasks(..)
  , Task(..)
  , processTasks
  , module Miniparse
  ) where

import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as LazyText

import           Data.Time                   (UTCTime (..), getCurrentTime,
                                              toGregorian)

import           GHC.IO.Exception            (IOException)

import           HTMLEntities.Decoder        (htmlEncodedText)

import           Network.HTTP.Client.Conduit (Request (..))
import           Network.HTTP.Simple

import           System.Directory            (doesFileExist)
import           System.IO                   (isEOF)
import           System.Timeout

import           AOC.SelectExample
import           Miniparse
import           Utils

readSession :: IO (Maybe Text)
readSession = fmap ttrim <$> readIfExists ".session-cookie"

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

dropAfterAll :: Text -> Text -> [Text]
dropAfterAll marker =
  map (Text.drop (Text.length marker) . snd) . Text.breakOnAll marker

dropBefore :: Text -> Text -> Text
dropBefore marker contents =
  case Text.breakOn marker contents of
    (result, rest)
      | Text.null rest ->
        error $
        "Could not find marker: " <>
        Text.unpack marker <> " in contents: " <> Text.unpack contents
      | otherwise -> result

htmlDecode :: Text -> Text
htmlDecode = LazyText.toStrict . LazyText.toLazyText . htmlEncodedText

removeTags :: Text -> Text
removeTags = Text.replace "<em>" "" . Text.replace "</em>" ""

codeBlocks :: Text -> [Text]
codeBlocks =
  map (removeTags . htmlDecode . dropBefore exampleEnd) .
  dropAfterAll exampleBegin
  where
    exampleBegin = "<pre><code>"
    exampleEnd = "</code></pre>"

getExample :: Integer -> Int -> IO Text
getExample year day =
  withCacheFile (".example-" <> tshow year <> "-" <> tshow day) $ do
    page <-
      simpleRequest $
      "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day
    pure $! selectExample year day $ codeBlocks page

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
  pure y

getInput :: Integer -> Int -> IO Text
getInput year day =
  withCacheFile (".input-" <> tshow year <> "-" <> tshow day) $
  simpleRequest $
  "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day <> "/input"

readIfExists :: Text -> IO (Maybe Text)
readIfExists fileName = do
  let fileName' = Text.unpack fileName
  exists <- doesFileExist fileName'
  if exists
    then Just <$> Text.readFile fileName'
    else pure Nothing

withCacheFile :: Text -> IO Text -> IO Text
withCacheFile fileName action = do
  existing <- readIfExists fileName
  case existing of
    Just contents -> pure contents
    Nothing -> do
      contents <- action
      Text.writeFile (Text.unpack fileName) contents
      pure contents

data Tasks where
  Tasks :: Integer -> Int -> Parser Text a -> [Task a] -> Tasks

data Task a where
  Task :: (Eq b, Show b) => (a -> b) -> b -> Task a
  Assert :: (Eq b, Show b) => String -> b -> b -> Task a
  AssertExample :: (Eq b, Show b) => String -> b -> (a -> b) -> Task a

taskName :: Task a -> String
taskName Task {}                  = "Task"
taskName (Assert name _ _)        = name
taskName (AssertExample name _ _) = name

taskTimeout :: Int -- seconds
taskTimeout = 40

processTasks :: Tasks -> IO ()
processTasks (Tasks year day parser tasks) = do
  Text.putStrLn $ "Year " <> tshow year <> ", day " <> tshow day
  forM_ tasks $ \task -> do
    result <- timeout (taskTimeout * 1000000) $ processTask year day parser task
    when (isNothing result) $
      error $
      taskName task <> ": timeout after " <> show taskTimeout <> " seconds"

processTask :: Integer -> Int -> Parser Text a -> Task a -> IO ()
processTask year day parser (Task solve expected) = do
  example <- justParse parser <$> getExample year day
  let exampleResult = solve example
  assertEqual "Example result" expected exampleResult
  input <- justParse parser <$> getInput year day
  let result = solve input
  print result
processTask _ _ _ (Assert message expected actual) =
  assertEqual message expected actual
processTask year day parser (AssertExample message expected fn) = do
  example <- justParse parser <$> getExample year day
  assertEqual ("Example " <> message) expected $ fn example

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual
  | expected == actual = putStrLn $ message <> " OK"
  | otherwise =
    error $
    message <> " is " <> show actual <> ", but expected " <> show expected
