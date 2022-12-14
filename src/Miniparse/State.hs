module Miniparse.State where

import           Control.Monad.State

import           Data.Char

import           Text.Read           (readEither)

import           Miniparse.Base

type StateParserT err src dest = StateT src (Either err) dest

type StateParser src dest = StateParserT String src dest

stateP ::
     (Eq src, Monoid src, Show src) => StateParser src dest -> Parser src dest
stateP p =
  Parser $
  runStateT p >=> \(result, remainder) ->
    if remainder == mempty
      then Right result
      else Left $ "unconsumed remainder: " ++ show remainder

unconsSP :: StateParserT e [a] (Maybe a)
unconsSP =
  state $ \case
    []     -> (Nothing, [])
    (x:xs) -> (Just x, xs)

unconsSP_ :: StateParser [a] a
unconsSP_ = unconsSP >>= maybe (failSP "unconsSP_: end of input") pure

ensureUnconsSP :: (Eq a, Show a) => Maybe a -> StateParser [a] ()
ensureUnconsSP expected =
  unconsSP >>= \actual ->
    if actual == expected
      then pure ()
      else failSP $ "expected " <> show expected <> " but got " <> show actual

ensureUnconsSP_ :: (Eq a, Show a) => a -> StateParser [a] ()
ensureUnconsSP_ = ensureUnconsSP . Just

putBackSP :: a -> StateParserT err [a] ()
putBackSP x = modify (x :)

failSP :: err -> StateParserT err src dest
failSP = lift . Left

readSP :: Read a => String -> StateParser src a
readSP = lift . readEither

unstateSP :: Monoid src => Parser src dest -> StateParser src dest
unstateSP p = StateT $ fmap (, mempty) . runParse p

naturalSP :: StateParser String Int
naturalSP = go >>= readSP
  where
    go =
      unconsSP >>= \case
        Nothing -> pure []
        (Just c)
          | isDigit c -> (c :) <$> go
          | otherwise -> putBackSP c >> pure []
