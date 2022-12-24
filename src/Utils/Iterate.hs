module Utils.Iterate where

import           Data.List

import           Utils.Base

type ItFn a = (a -> a) -> a -> [a]

type ItFnL a = (a -> a) -> a -> a

iterateN :: Int -> ItFn a
iterateN n f = take (succ n) . iterate' f

iterateNL :: Int -> ItFnL a
iterateNL n f = last . iterateN n f

iterateWhile :: (a -> Bool) -> ItFn a
iterateWhile c fn v
  | c v = v : iterateWhile c fn (fn v)
  | otherwise = []

iterateWhileL :: (a -> Bool) -> ItFnL a
iterateWhileL c f = last . iterateWhile c f

iterateWhile2 :: (a -> a -> Bool) -> ItFn a
iterateWhile2 c fn v = v : vs
  where
    v' = fn v
    vs
      | c v v' = iterateWhile2 c fn v'
      | otherwise = []

iterateWhile2L :: (a -> a -> Bool) -> ItFnL a
iterateWhile2L c f = last . iterateWhile2 c f

iterateSettle :: Eq a => ItFn a
iterateSettle = iterateWhile2 (/=)

iterateSettleL :: Eq a => ItFnL a
iterateSettleL f = last . iterateSettle f

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = unfoldr (fmap dupe . f)

iterateMaybeL :: (a -> Maybe a) -> a -> a
iterateMaybeL f = last . iterateMaybe f

iterateEither :: (a -> Either b a) -> a -> b
iterateEither fn v =
  case fn v of
    Left b  -> b
    Right v -> iterateEither fn v
