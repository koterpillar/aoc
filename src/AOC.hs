{-# LANGUAGE GADTs #-}

module AOC
  ( getExample
  , getInput
  , ExampleScraper(..)
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

codeBlockBegin :: Text
codeBlockBegin = "<pre><code>"

codeBlockEnd :: Text
codeBlockEnd = "</code></pre>"

codeBlocks :: Text -> [Text]
codeBlocks =
  map (removeTags . htmlDecode . dropBefore codeBlockEnd) .
  dropAfterAll codeBlockBegin

inlineCode :: Text -> [Text]
inlineCode =
  filter ((<) 10 . Text.length) .
  map (removeTags . htmlDecode . dropBefore exampleEnd) .
  dropAfterAll inlineCodeBegin . Text.replace codeBlockBegin ""
  where
    inlineCodeBegin = "<code>"
    exampleEnd = "</code>"

data ExampleScraper
  = ShowExamples
  | CodeBlock Int
  | LastCodeBlock
  | InlineCode Int
  | LastInlineCode
  | Inline Text
  deriving (Show)

scraperCacheName :: ExampleScraper -> Text
scraperCacheName = Text.replace " " "-" . Text.replace "\"" "" . tshow

getExample :: Integer -> Int -> ExampleScraper -> IO Text
getExample year day scraper =
  withCacheFile
    (".example-" <>
     tshow year <> "-" <> tshow day <> "-" <> scraperCacheName scraper) $ do
    page <-
      simpleRequest $
      "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day
    pure $! selectExample scraper page

showExamples :: Text -> Text
showExamples page =
  Text.unlines $ "Examples found:" : codeBlocksMsg ++ inlineCodeMsg
  where
    codeBlocksMsg = labelExamples "CodeBlock" $ codeBlocks page
    inlineCodeMsg = labelExamples "InlineCode" $ inlineCode page
    labelExamples itemName =
      zipWith
        (\i e -> "--- " <> itemName <> " " <> tshow i <> " ---\n" <> e)
        [0 ..]

selectExample :: ExampleScraper -> Text -> Text
selectExample ShowExamples   = error . Text.unpack . showExamples
selectExample (CodeBlock n)  = (!! n) . codeBlocks
selectExample LastCodeBlock  = last . codeBlocks
selectExample (InlineCode n) = (!! n) . inlineCode
selectExample LastInlineCode = last . inlineCode
selectExample (Inline text)  = const text

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
  Tasks
    :: Integer -> Int -> ExampleScraper -> Parser Text a -> [Task a] -> Tasks

data Task a where
  Task :: (Eq b, Show b) => (a -> b) -> b -> Task a
  TaskScraper :: (Eq b, Show b) => ExampleScraper -> (a -> b) -> b -> Task a
  Assert :: (Eq b, Show b) => String -> b -> b -> Task a
  AssertExample :: (Eq b, Show b) => String -> b -> (a -> b) -> Task a

taskName :: Task a -> String
taskName TaskScraper {}           = "Task"
taskName Task {}                  = "Task"
taskName (Assert name _ _)        = name
taskName (AssertExample name _ _) = name

taskTimeout :: Int -- seconds
taskTimeout = 40

processTasks :: Tasks -> IO ()
processTasks (Tasks year day scraper parser tasks) = do
  Text.putStrLn $ "Year " <> tshow year <> ", day " <> tshow day
  forM_ tasks $ \task -> do
    result <-
      timeout (taskTimeout * 1000000) $ processTask year day scraper parser task
    when (isNothing result) $
      error $
      taskName task <> ": timeout after " <> show taskTimeout <> " seconds"

processTask ::
     Integer -> Int -> ExampleScraper -> Parser Text a -> Task a -> IO ()
processTask year day _ parser (TaskScraper scraper solve expected) =
  processTask year day scraper parser (Task solve expected)
processTask year day scraper parser (Task solve expected) = do
  example <- justParse parser <$> getExample year day scraper
  let exampleResult = solve example
  assertEqual "Example result" expected exampleResult
  input <- justParse parser <$> getInput year day
  let result = solve input
  print result
processTask _ _ _ _ (Assert message expected actual) =
  assertEqual message expected actual
processTask year day scraper parser (AssertExample message expected fn) = do
  example <- justParse parser <$> getExample year day scraper
  assertEqual ("Example " <> message) expected $ fn example

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual
  | expected == actual = putStrLn $ message <> " OK"
  | otherwise =
    error $
    message <> " is " <> show actual <> ", but expected " <> show expected
