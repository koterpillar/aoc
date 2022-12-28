{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module AOC
  ( ExampleScraper(..)
  , Tasks(..)
  , Task
  , assert
  , assertExample
  , task
  , taskScraper
  , processTasks
  , module Miniparse
  ) where

import           Control.Monad.Free
import           Control.Monad.Free.TH

import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as LazyText

import           Data.String                 (IsString (..))

import           Data.Time                   (UTCTime (..), getCurrentTime,
                                              toGregorian)

import           GHC.IO.Exception            (IOException)

import           HTMLEntities.Decoder        (htmlEncodedText)

import           Network.HTTP.Client.Conduit (Request (..))
import           Network.HTTP.Simple

import           System.Directory            (createDirectoryIfMissing,
                                              doesFileExist)
import           System.FilePath             (takeDirectory)
import           System.IO                   (isEOF)
import           System.Timeout

import           Miniparse
import           Utils

-- | https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/
userAgent :: Text
userAgent = "github.com/koterpillar/aoc by a@koterpillar.com"

readIfExists :: Text -> IO (Maybe Text)
readIfExists fileName = do
  let fileName' = Text.unpack fileName
  exists <- doesFileExist fileName'
  if exists
    then Just <$> Text.readFile fileName'
    else pure Nothing

readSession :: IO (Maybe Text)
readSession = fmap ttrim <$> readIfExists ".session-cookie"

addHeader :: String -> Text -> Request -> Request
addHeader name value request =
  request
    { requestHeaders =
        (fromString name, Text.encodeUtf8 value) : requestHeaders request
    }

simpleRequest :: Text -> IO Text
simpleRequest url = do
  request <- parseRequestThrow $ Text.unpack url
  session <- readSession
  let request' =
        addHeader "User-Agent" userAgent $
        foldr (addHeader "Cookie" . ("session=" <>)) request session
  response <- getResponseBody <$> httpBS request'
  pure $ Text.decodeUtf8 response

class CacheFileName a where
  cacheFileName :: a -> Text

withCacheFile' ::
     CacheFileName a
  => Integer
  -> Int
  -> a
  -> (Maybe Text -> IO Text)
  -> IO Text
withCacheFile' year day cacheKey action = do
  let fileName =
        ".cache/" <>
        tshow year <> "/" <> tshow day <> "/" <> cacheFileName cacheKey
  cached <- readIfExists fileName
  result <- action cached
  when (Just result /= cached) $ do
    let fileName' = Text.unpack fileName
    createDirectoryIfMissing True $ takeDirectory fileName'
    Text.writeFile fileName' result
  pure result

withCacheFile :: CacheFileName a => Integer -> Int -> a -> IO Text -> IO Text
withCacheFile year day cacheKey action =
  withCacheFile' year day cacheKey $ \case
    Nothing     -> action
    Just cached -> pure cached

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
selectExample ShowExamples   = terror . showExamples
selectExample (CodeBlock n)  = (!! n) . codeBlocks
selectExample LastCodeBlock  = last . codeBlocks
selectExample (InlineCode n) = (!! n) . inlineCode
selectExample LastInlineCode = last . inlineCode
selectExample (Inline text)  = const text

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
  pure y

data Input =
  Input
  deriving (Eq, Ord, Show)

instance CacheFileName Input where
  cacheFileName Input = "input"

ioGetInput :: Integer -> Int -> IO Text
ioGetInput year day =
  withCacheFile year day Input $
  simpleRequest $
  "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day <> "/input"

data Instructions =
  Instructions
  deriving (Eq, Ord, Show)

instance CacheFileName Instructions where
  cacheFileName Instructions = "instructions"

ioGetInstructions :: Integer -> Int -> IO Text
ioGetInstructions year day =
  withCacheFile year day Instructions $
  simpleRequest $
  "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day

data TaskF a param
  = GetParser (Parser Text a -> param)
  | GetScraper (ExampleScraper -> param)
  | GetInput (Text -> param)
  | GetInstructions (Text -> param)
  | TLiftIO (IO ()) param
  deriving (Functor)

makeFree ''TaskF

data Tasks where
  Tasks
    :: Integer -> Int -> ExampleScraper -> Parser Text a -> [Task a] -> Tasks

type Task a = Free (TaskF a) ()

task :: (Eq b, Show b) => (a -> b) -> b -> Task a
task = task' Nothing

taskScraper :: (Eq b, Show b) => ExampleScraper -> (a -> b) -> b -> Task a
taskScraper = task' . Just

getExample :: Maybe ExampleScraper -> Free (TaskF a) Text
getExample maybeScraper = do
  scraper <- maybe getScraper pure maybeScraper
  selectExample scraper <$> getInstructions

task' :: (Eq b, Show b) => Maybe ExampleScraper -> (a -> b) -> b -> Task a
task' scraper solve expected = do
  parser <- getParser
  example <- justParse parser <$> getExample scraper
  let exampleResult = solve example
  tLiftIO $ assertEqual "Example result" expected exampleResult
  input <- justParse parser <$> getInput
  let result = solve input
  tLiftIO $ print result

assert :: (Eq b, Show b) => Text -> b -> b -> Task a
assert message expected actual = tLiftIO $ assertEqual message expected actual

assertExample :: (Eq b, Show b) => Text -> b -> (a -> b) -> Task a
assertExample message expected fn = do
  parser <- getParser
  example <- justParse parser <$> getExample Nothing
  tLiftIO $ assertEqual ("Example " <> message) expected $ fn example

taskTimeout :: Int -- seconds
taskTimeout = 40

processTask ::
     Integer -> Int -> ExampleScraper -> Parser Text a -> TaskF a (IO b) -> IO b
processTask _ _ _ parser (GetParser f)       = f parser
processTask _ _ scraper _ (GetScraper f)     = f scraper
processTask year day _ _ (GetInput f)        = ioGetInput year day >>= f
processTask year day _ _ (GetInstructions f) = ioGetInstructions year day >>= f
processTask _ _ _ _ (TLiftIO act f)          = act >> f

processTasks :: Tasks -> IO ()
processTasks (Tasks year day scraper parser tasks) = do
  Text.putStrLn $ "Year " <> tshow year <> ", day " <> tshow day
  for_ tasks $ \task -> do
    result <-
      timeout (taskTimeout * 1000000) $
      iterM (processTask year day scraper parser) task
    when (isNothing result) $
      terror $ "Task: timeout after " <> tshow taskTimeout <> " seconds"

assertEqual :: (Eq a, Show a) => Text -> a -> a -> IO ()
assertEqual message expected actual
  | expected == actual = Text.putStrLn $ message <> " OK"
  | otherwise =
    terror $
    message <> " is " <> tshow actual <> ", but expected " <> tshow expected
