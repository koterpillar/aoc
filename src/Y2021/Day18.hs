module Y2021.Day18 where

import           Control.Monad.State
import qualified Data.Text           as Text

import           AOC
import           Utils

data Snail a
  = PNumber Int
  | PPair (Snail a) (Snail a)
  deriving (Ord, Eq)

instance Show a => Show (Snail a) where
  show (PNumber n)   = show n
  show (PPair n1 n2) = "[" <> show n1 <> "," <> show n2 <> "]"

data SnailCtx a
  = SCRoot
  | SCLeft (SnailCtx a) (Snail a)
  | SCRight (Snail a) (SnailCtx a)
  deriving (Ord, Eq)

instance Show a => Show (SnailCtx a) where
  show SCRoot          = "@"
  show (SCLeft ctx r)  = "[" <> show ctx <> "," <> show r <> "]"
  show (SCRight l ctx) = "[" <> show l <> "," <> show ctx <> "]"

type SLoc a = (Snail a, SnailCtx a)

sToLoc :: Snail a -> SLoc a
sToLoc s = (s, SCRoot)

scUp, scDownLeft, scDownRight :: Snail a -> SnailCtx a -> Maybe (SLoc a)
scUp _ SCRoot          = Nothing
scUp l (SCLeft ctx r)  = Just (PPair l r, ctx)
scUp r (SCRight l ctx) = Just (PPair l r, ctx)

sFromLoc :: Snail a -> SnailCtx a -> Snail a
sFromLoc s SCRoot          = s
sFromLoc l (SCLeft ctx r)  = sFromLoc (PPair l r) ctx
sFromLoc r (SCRight l ctx) = sFromLoc (PPair l r) ctx

scDownLeft (PNumber _) _   = Nothing
scDownLeft (PPair l r) ctx = Just (l, SCLeft ctx r)

scDownRight (PNumber _) _   = Nothing
scDownRight (PPair l r) ctx = Just (r, SCRight l ctx)

type SnailInt = Snail Int

type SLocInt = SLoc Int

magnitude :: SnailInt -> Int
magnitude (PNumber n)   = n
magnitude (PPair n1 n2) = 3 * magnitude n1 + 2 * magnitude n2

snadd :: SnailInt -> SnailInt -> SnailInt
snadd n1 n2 = snreduce $ PPair n1 n2

snexplode :: SnailInt -> Maybe SnailInt
snexplode =
  fmap (uncurry sFromLoc . uncurry afterExplode) . findExploding 0 . sToLoc

type AfterExplode = (Maybe Int, Maybe Int)

afterExplode :: SLocInt -> AfterExplode -> SLocInt
afterExplode (t, SCRight l ctx) (Just n, rr) =
  afterExplode (t, SCRight (addToRightmost n l) ctx) (Nothing, rr)
afterExplode (t, SCLeft ctx r) (rl, Just n) =
  afterExplode (t, SCLeft ctx (addToLeftmost n r)) (rl, Nothing)
afterExplode l@(t, ctx) ae =
  case scUp t ctx of
    Nothing -> l
    Just l' -> afterExplode l' ae

findExploding :: Int -> SLocInt -> Maybe (SLocInt, AfterExplode)
findExploding _ (PNumber _, _) = Nothing
findExploding d (PPair (PNumber nl) (PNumber nr), ctx)
  | d >= 4 = Just ((PNumber 0, ctx), (Just nl, Just nr))
  | otherwise = Nothing
findExploding d (t, ctx) =
  let d' = d + 1
   in (scDownLeft t ctx >>= findExploding d') <|>
      (scDownRight t ctx >>= findExploding d')

addToLeftmost :: Int -> SnailInt -> SnailInt
addToLeftmost n (PNumber n') = PNumber (n + n')
addToLeftmost n (PPair l r)  = PPair (addToLeftmost n l) r

addToRightmost :: Int -> SnailInt -> SnailInt
addToRightmost n (PNumber n') = PNumber (n + n')
addToRightmost n (PPair l r)  = PPair l (addToLeftmost n r)

snsplit :: SnailInt -> Maybe SnailInt
snsplit = fmap (uncurry sFromLoc) . go . sToLoc
  where
    go :: SLocInt -> Maybe SLocInt
    go (t@PPair {}, c) = (scDownLeft t c >>= go) <|> (scDownRight t c >>= go)
    go (PNumber n, c)
      | n < 10 = Nothing
      | otherwise =
        let n' = fromIntegral n / 2
            t' = PPair (PNumber $ floor n') (PNumber $ ceiling n')
         in Just (t', c)

snreduce :: SnailInt -> SnailInt
snreduce t = (snreduce <$> snexplode t <|> snreduce <$> snsplit t) & fromMaybe t

snsum :: [SnailInt] -> SnailInt
snsum = foldl1 snadd

part1 :: [SnailInt] -> Int
part1 = magnitude . snsum

tasks =
  Tasks
    2021
    18
    (linesP &** pureP parse)
    [ Assert "split 9" Nothing (snsplit $ PNumber 9)
    , Assert "split 11" (Just $ parse "[5,6]") (snsplit $ PNumber 11)
    , Assert "split 12" (Just $ parse "[6,6]") (snsplit $ PNumber 12)
    , Assert
        "explode"
        (Just $ parse "[[[[0,9],2],3],4]")
        (snexplode $ parse "[[[[[9,8],1],2],3],4]")
    , AssertExample
        "sum"
        (parse "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
        snsum
    , Task part1 4140
    ]

parseS :: State String SnailInt
parseS = do
  c <- getchar
  case c of
    '[' -> do
      n1 <- parseS
      ensure ','
      n2 <- parseS
      ensure ']'
      return $ PPair n1 n2
    d
      | isDigit d -> do
        n <- parseNumber
        return $ PNumber $ read $ d : n
      | otherwise -> sterror $ "expected number or [, got " <> show c

parse :: Text -> SnailInt
parse input =
  let (number, []) = runState parseS (Text.unpack input)
   in number

getchar :: State String Char
getchar = state $ fromJust . uncons

sterror :: String -> State String a
sterror message = do
  rem <- get
  error $ message <> " remainder: " <> show rem

ensure :: Char -> State String ()
ensure c =
  getchar >>= \c' ->
    if c == c'
      then pure ()
      else sterror $ "expected " <> show c <> " but got " <> show c'

parseNumber :: State String [Char]
parseNumber =
  gets head >>= \c ->
    if isDigit c
      then (c :) <$> parseNumber
      else pure []
