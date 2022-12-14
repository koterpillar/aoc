module Utils.Iterate where

type ItFn a = (a -> a) -> a -> [a]

type ItFnL a = (a -> a) -> a -> a

iterateN :: Int -> ItFn a
iterateN 0 _ v = [v]
iterateN n fn v =
  let !u = iterateN (n - 1) fn v
   in v : u

iterateNL :: Int -> ItFnL a
iterateNL n f = last . iterateN n f

iterateWhile :: (a -> Bool) -> ItFn a
iterateWhile c fn v
  | c v =
    let v' = fn v
     in v : iterateWhile c fn v'
  | otherwise = []

iterateWhileL :: (a -> Bool) -> ItFnL a
iterateWhileL c f = last . iterateWhile c f

iterateSettle :: Eq a => ItFn a
iterateSettle fn v =
  if v == v'
    then [v]
    else v : iterateSettle fn v'
  where
    v' = fn v

iterateSettleL :: Eq a => ItFnL a
iterateSettleL f = last . iterateSettle f

iterateEither :: (a -> Either b a) -> a -> b
iterateEither fn v =
  case fn v of
    Left b  -> b
    Right v -> iterateEither fn v
