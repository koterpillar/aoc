module Miniparse.Brackets
  ( Brackets(..)
  , MatchingBrackets
  , charBrackets
  , bracketsP
  , bracketsSPT
  ) where

import           Control.Monad.State

import           Miniparse.Base
import           Miniparse.State

import           Utils

data Brackets a
  = BR a a [Brackets a]
  | BE a
  deriving (Ord, Eq)

instance Show a => Show (Brackets a) where
  showsPrec _ (BR b1 b2 contents) = shows b1 . showList contents . shows b2

type MatchingBrackets a = [(a, a)]

charBrackets :: MatchingBrackets Char
charBrackets = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

data BracketStatus a
  = Ordinary
  | Opening
  | Closing a
  deriving (Ord, Eq, Show)

bracketStatus :: Eq a => MatchingBrackets a -> a -> BracketStatus a
bracketStatus [] _ = Ordinary
bracketStatus ((b1, b2):bs) c
  | c == b1 = Opening
  | c == b2 = Closing b1
  | otherwise = bracketStatus bs c

bracketsP :: (Eq a, Show a) => MatchingBrackets a -> Parser [a] [Brackets a]
bracketsP match = stateP $ mapStateT (mapLeft show) $ bracketsSPT match

data Mismatch a =
  Mismatch (Maybe a) (Maybe a)
  deriving (Eq, Ord, Show)

type BracketParser a r = StateParserT (Mismatch a) [a] r

mismatch :: Maybe a -> Maybe a -> BracketParser a r
mismatch b1 b2 = lift $ Left $ Mismatch b1 b2

bracketsSPT :: Eq a => MatchingBrackets a -> BracketParser a [Brackets a]
bracketsSPT match =
  unconsSP >>= \case
    Nothing -> pure []
    Just c ->
      case bracketStatus match c of
        Ordinary -> do
          rest <- bracketsSPT match
          pure $ BE c : rest
        Closing b -> mismatch Nothing (Just c)
        Opening -> do
          (insideCb, inside) <- bracketsSPT1 match c
          rest <- bracketsSPT match
          pure $ BR c insideCb inside : rest

bracketsSPT1 ::
     Eq a => MatchingBrackets a -> a -> BracketParser a (a, [Brackets a])
bracketsSPT1 match opn =
  unconsSP >>= \case
    Nothing -> mismatch (Just opn) Nothing
    Just c ->
      case bracketStatus match c of
        Ordinary -> do
          (cb, rest) <- bracketsSPT1 match opn
          pure (cb, BE c : rest)
        Closing b
          | b == opn -> pure (c, [])
          | otherwise -> mismatch (Just opn) (Just c)
        Opening -> do
          (insideCb, inside) <- bracketsSPT1 match c
          (cb, rest) <- bracketsSPT1 match opn
          pure (cb, BR c insideCb inside : rest)
