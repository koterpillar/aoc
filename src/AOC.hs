{-# LANGUAGE GADTs #-}

module AOC
  ( getExampleY
  , getExample
  , getInputY
  , getInput
  , Tasks(..)
  , Task(..)
  , processTasks
  ) where

import           Control.Exception           (catch)
import           Control.Monad               (when)

import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as LazyText

import           Data.Time                   (UTCTime (..), getCurrentTime,
                                              toGregorian)

import           Debug.Trace

import           GHC.IO.Exception            (IOException)

import           HTMLEntities.Decoder        (htmlEncodedText)

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

getExampleY :: Integer -> Int -> IO Text
getExampleY year day =
  withCacheFile (".example-" <> tshow year <> "-" <> tshow day) $ do
    page <-
      simpleRequest $
      "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day
    pure $ selectExample year day $ codeBlocks page

selectExample :: Integer -> Int -> [Text] -> Text
selectExample _ _ [example] = example
selectExample 2021 4 examples = head examples
selectExample 2021 5 examples = head examples
selectExample 2021 6 examples = head examples
selectExample 2021 8 examples = examples !! 2
selectExample 2021 9 examples = head examples
selectExample 2021 10 examples = last examples
selectExample 2021 11 examples = head examples
selectExample 2021 12 examples = head examples
selectExample year day examples =
  error $
  "Cannot select from " <>
  show (length examples) <>
  " examples for year " <>
  show year <>
  " day " <>
  show day <> ":\n\n" <> Text.unpack (Text.intercalate "\n---\n" examples)

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

data Tasks where
  Tasks :: Integer -> Int -> (Text -> a) -> [Task a] -> Tasks

data Task a where
  Task :: (Eq b, Show b) => (a -> b) -> b -> Task a
  Assert :: (Eq b, Show b) => String -> b -> b -> Task a
  AssertExample :: (Eq b, Show b) => String -> b -> (a -> b) -> Task a

processTasks :: Tasks -> IO ()
processTasks (Tasks year day parse tasks) =
  traverse_ (processTask year day parse) tasks

processTask :: Integer -> Int -> (Text -> a) -> Task a -> IO ()
processTask year day parse (Task solve expected) = do
  example <- parse <$> getExampleY year day
  let exampleResult = solve example
  assertEqual "Example result" expected exampleResult
  input <- parse <$> getInputY year day
  let result = solve input
  print result
processTask _ _ _ (Assert message expected actual) =
  assertEqual message expected actual
processTask year day parse (AssertExample message expected fn) = do
  example <- parse <$> getExampleY year day
  assertEqual "Example result" expected $ fn example
