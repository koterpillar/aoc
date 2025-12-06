{-# LANGUAGE GADTs #-}

module AOC (
    getExample,
    getInput,
    ExampleScraper (..),
    Tasks (..),
    Task (..),
    task,
    taskBlind,
    taskPart,
    taskScraper,
    taskTimeout,
    processTasks,
    module Miniparse,
) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText

import Data.String (IsString (..))

import Data.Time (
    UTCTime (..),
    getCurrentTime,
    toGregorian,
 )

import GHC.IO.Exception (IOException)

import HTMLEntities.Decoder (htmlEncodedText)

import Network.HTTP.Client.Conduit (Request (..))
import Network.HTTP.Simple

import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.FilePath (takeDirectory)
import System.IO (isEOF)
import System.Timeout

import Miniparse
import Utils

-- | https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/
userAgent :: Text
userAgent = "github.com/koterpillar/aoc by a@koterpillar.com"

baseURL :: Integer -> Int -> Text
baseURL year day =
    "https://adventofcode.com/" <> tshow year <> "/day/" <> tshow day

baseRequest :: Integer -> Int -> Text -> IO Request
baseRequest year day path = parseRequestThrow $ Text.unpack $ baseURL year day <> path

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

aocRequest :: Integer -> Int -> Text -> IO Text
aocRequest year day path = baseRequest year day path >>= aocRequest'

aocRequest' :: Request -> IO Text
aocRequest' request = do
    session <- readSession
    let request' =
            addHeader "User-Agent" userAgent $
                foldr (addHeader "Cookie" . ("session=" <>)) request session
    response <- getResponseBody <$> httpBS request'
    pure $ Text.decodeUtf8 response

fromMaybeIO :: IO a -> Maybe a -> IO a
fromMaybeIO = flip maybe pure

class CacheFileName a where
    cacheFileName :: a -> Text

withCacheFile' ::
    (CacheFileName a) =>
    Integer ->
    Int ->
    a ->
    (Maybe Text -> IO Text) ->
    IO Text
withCacheFile' year day cacheKey action = do
    let fileName =
            ".cache/"
                <> tshow year
                <> "/"
                <> tshow day
                <> "/"
                <> cacheFileName cacheKey
    cached <- readIfExists fileName
    result <- action cached
    when (Just result /= cached) $ do
        let fileName' = Text.unpack fileName
        createDirectoryIfMissing True $ takeDirectory fileName'
        Text.writeFile fileName' result
    pure result

withCacheFile :: (CacheFileName a) => Integer -> Int -> a -> IO Text -> IO Text
withCacheFile year day cacheKey = withCacheFile' year day cacheKey . fromMaybeIO

dropAfterAll :: Text -> Text -> [Text]
dropAfterAll marker =
    map (Text.drop (Text.length marker) . snd) . Text.breakOnAll marker

dropBefore :: Text -> Text -> Text
dropBefore marker contents =
    case Text.breakOn marker contents of
        (result, rest)
            | Text.null rest ->
                error $
                    "Could not find marker: "
                        <> Text.unpack marker
                        <> " in contents: "
                        <> Text.unpack contents
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
    map (removeTags . htmlDecode . dropBefore codeBlockEnd)
        . dropAfterAll codeBlockBegin

inlineCode :: Text -> [Text]
inlineCode =
    filter ((<) 10 . Text.length)
        . map (removeTags . htmlDecode . dropBefore exampleEnd)
        . dropAfterAll inlineCodeBegin
        . Text.replace codeBlockBegin ""
  where
    inlineCodeBegin = "<code>"
    exampleEnd = "</code>"

answerBegin :: Text
answerBegin = "<p>Your puzzle answer was <code>"

answerEnd :: Text
answerEnd = "</code>"

answers :: Text -> [Text]
answers = map (dropBefore answerEnd) . dropAfterAll answerBegin

data ExampleScraper
    = ShowExamples
    | CodeBlock Int
    | LastCodeBlock
    | InlineCode Int
    | LastInlineCode
    | Inline Text
    deriving (Show)

instance CacheFileName ExampleScraper where
    cacheFileName = ("example-" <>) . Text.replace " " "-" . terase "\"" . tshow

getExample :: Integer -> Int -> ExampleScraper -> IO Text
getExample year day scraper =
    withCacheFile year day scraper $ do
        page <- aocRequest year day ""
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
selectExample ShowExamples = terror . showExamples
selectExample (CodeBlock n) = indexE "code blocks" n . codeBlocks
selectExample LastCodeBlock = last . codeBlocks
selectExample (InlineCode n) = indexE "inline code" n . inlineCode
selectExample LastInlineCode = last . inlineCode
selectExample (Inline text) = const text

newtype Answer = Answer {answerPart :: Int}
    deriving (Eq, Ord, Show)

instance CacheFileName Answer where
    cacheFileName (Answer part) = "answer-" <> tshow part

getAnswer :: Integer -> Int -> Answer -> IO (Maybe Text)
getAnswer year day answer =
    discardEmpty <$> withCacheFile' year day answer fetchAnswer
  where
    fetchAnswer = fromMaybeIO fetchAnswer' . ((=<<) discardEmpty)
    fetchAnswer' = do
        page <- aocRequest year day ""
        let as = answers page
        let a = fromMaybe mempty $ as !? pred (answerPart answer)
        pure $! a
    discardEmpty t
        | Text.null t = Nothing
        | otherwise = Just t

newtype Tried = Tried {triedPart :: Int}
    deriving (Eq, Ord, Show)

instance CacheFileName Tried where
    cacheFileName (Tried part) = "tried-" <> tshow part

submitAnswer :: Integer -> Int -> Answer -> Text -> IO ()
submitAnswer year day (Answer part) result =
    void $ withCacheFile' year day (Tried part) go
  where
    fromCache = maybe Set.empty (Set.fromList . Text.lines)
    toCache = Text.unlines . Set.toList
    go maybeTried = do
        let tried = fromCache maybeTried
        if Set.member result tried
            then terror "Answer already tried."
            else do
                resp <- postAnswer
                case resp of
                    True -> pure $ toCache tried
                    False -> pure $ toCache $ Set.insert result tried
    postAnswer = do
        request' <- parseRequestThrow (Text.unpack $ baseURL year day <> "/answer")
        let request =
                request'
                    & setRequestBodyURLEncoded
                        [ ("level", Text.encodeUtf8 $ tshow part)
                        , ("answer", Text.encodeUtf8 result)
                        ]
        page <- aocRequest' request
        if Text.isInfixOf "That's the right answer" page
            then do
                Text.putStrLn "Submitted OK"
                pure True
            else
                if Text.isInfixOf "That's not the right answer" page
                    then do
                        Text.putStrLn "Submitted KO"
                        -- FIXME: Error after this
                        pure False
                    else do
                        Text.putStrLn "Unexpected response:"
                        terror page

currentYear :: IO Integer
currentYear = do
    (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
    pure y

data Input
    = Input
    deriving (Eq, Ord, Show)

instance CacheFileName Input where
    cacheFileName Input = "input"

getInput :: Integer -> Int -> IO Text
getInput year day =
    withCacheFile year day Input $ aocRequest year day "/input"

data Tasks where
    Tasks ::
        Integer -> Int -> ExampleScraper -> Parser Text a -> [Task a] -> Tasks

class
    (Eq a) =>
    AnswerLike a
    where
    answerToText :: a -> Text

instance AnswerLike Text where
    answerToText = id

instance AnswerLike String where
    answerToText = Text.pack

instance AnswerLike Int where
    answerToText = Text.pack . show

instance AnswerLike Integer where
    answerToText = Text.pack . show

instance AnswerLike () where
    answerToText () = mempty

data Task a where
    Task :: (AnswerLike b) => (a -> b) -> b -> Task a
    Task' ::
        (AnswerLike b) =>
        { _taskFn :: a -> b
        , _taskExampleAnswer :: Maybe b
        , _taskScraper :: Maybe ExampleScraper
        , _taskPart :: Maybe Int
        , _taskTimeout :: Maybe Int
        } ->
        Task a
    Assert :: (Eq b, Show b) => Text -> b -> b -> Task a
    AssertExample :: (Eq b, Show b) => Text -> b -> (a -> b) -> Task a

task :: (AnswerLike b) => (a -> b) -> b -> Task a
task _taskFn a = Task'{..}
  where
    _taskExampleAnswer = Just a
    _taskScraper = Nothing
    _taskPart = Nothing
    _taskTimeout = Nothing

taskBlind :: (AnswerLike b) => (a -> b) -> Task a
taskBlind _taskFn = Task'{..}
  where
    _taskExampleAnswer = Nothing
    _taskScraper = Nothing
    _taskPart = Nothing
    _taskTimeout = Nothing

taskScraper :: ExampleScraper -> Task a -> Task a
taskScraper scraper t@Task'{} = t{_taskScraper = Just scraper}
taskScraper _ t = terror $ "Cannot override scraper for: " <> taskName t

taskPart :: Int -> Task a -> Task a
taskPart part t@Task'{} = t{_taskPart = Just part}
taskPart _ t = terror $ "Cannot override part for: " <> taskName t

taskNoPart :: Task a -> Task a
taskNoPart t@Task'{} = t{_taskPart = Nothing}
taskNoPart t = terror $ "Cannot remove part from: " <> taskName t

taskTimeout :: Int -> Task a -> Task a
taskTimeout timeout t@Task'{} = t{_taskTimeout = Just timeout}
taskTimeout _ t = terror $ "Cannot override timeout for: " <> taskName t

taskName :: Task a -> Text
taskName Task{} = "Task"
taskName Task'{} = "Task"
taskName (Assert name _ _) = name
taskName (AssertExample name _ _) = name

getTimeout :: Task a -> Int -- seconds
getTimeout (Task' _ _ _ _ (Just t)) = t
getTimeout _ = 40

processTasks :: Tasks -> IO ()
processTasks (Tasks year day scraper parser tasks) = do
    Text.putStrLn $ "Year " <> tshow year <> ", day " <> tshow day
    for_ tasks $ \task -> do
        let timeoutValue = getTimeout task
        result <-
            timeout (timeoutValue * 1000000) $
                processTask year day scraper parser task
        when (isNothing result) $
            terror $
                taskName task <> ": timeout after " <> tshow timeoutValue <> " seconds"

processTask ::
    Integer -> Int -> ExampleScraper -> Parser Text a -> Task a -> IO ()
processTask year day globalScraper parser (Task solve expected) =
    processTask
        year
        day
        globalScraper
        parser
        (Task' solve (Just expected) Nothing Nothing Nothing)
processTask year day globalScraper parser (Task' solve exampleAnswer taskScraper part _) = do
    let scraper = fromMaybe globalScraper taskScraper
    example <- justParse parser <$> getExample year day scraper
    let exampleResult = solve example
    case exampleAnswer of
        Just answer ->
            assertEqual
                "Example result"
                (answerToText answer)
                (answerToText exampleResult)
        Nothing -> Text.putStrLn $ "Example result " <> answerToText exampleResult
    input <- justParse parser <$> getInput year day
    let result = solve input
    Text.putStrLn $ answerToText result
    for_ part $ \part' -> do
        getAnswer year day (Answer part') >>= \case
            Just answer -> assertEqual ("Answer part " <> tshow part') answer (answerToText result)
            Nothing -> submitAnswer year day (Answer part') (answerToText result)
processTask _ _ _ _ (Assert message expected actual) =
    assertEqual message expected actual
processTask year day scraper parser (AssertExample message expected fn) = do
    example <- justParse parser <$> getExample year day scraper
    assertEqual ("Example " <> message) expected $ fn example

assertEqual :: (Eq a, Show a) => Text -> a -> a -> IO ()
assertEqual message expected actual
    | expected == actual = Text.putStrLn $ message <> " OK"
    | otherwise =
        terror $
            message <> " is " <> tshow actual <> ", but expected " <> tshow expected
