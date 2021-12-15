module Miniparse where

import qualified Data.Text as Text

import           Text.Read (readEither)

import           Utils

newtype Parser src dest =
  Parser
    { runParse :: src -> Either String dest
    }

instance Functor (Parser src) where
  fmap f (Parser p) = Parser $ fmap f . p

pureP :: (src -> dest) -> Parser src dest
pureP = Parser . (pure .)

justParse :: Parser src dest -> src -> dest
justParse parser src =
  case runParse parser src of
    Left err   -> error err
    Right dest -> dest

choiceP :: Eq src => [(src, dest)] -> Parser src dest
choiceP _ = undefined

linesP :: Parser Text [Text]
linesP = pureP Text.lines

wordsP :: Parser Text [Text]
wordsP = pureP Text.words

splitP :: Eq a => [a] -> Parser [a] [[a]]
splitP sep = pureP $ splitOn sep

tsplitP :: Text -> Parser Text [Text]
tsplitP sep = pureP $ Text.splitOn sep

readP :: Read a => Parser Text a
readP = Parser $ readEither . Text.unpack

integerP :: Parser Text Int
integerP = readP

integersP :: Text -> Parser Text [Int]
integersP sep = tsplitP sep &** integerP

charactersP :: Parser Text [Text]
charactersP = pureP $ map Text.singleton . Text.unpack

digitsP :: Parser Text [Int]
digitsP = charactersP &** integerP

(&*) :: Parser a b -> Parser b c -> Parser a c
Parser p1 &* Parser p2 = Parser $ p1 >=> p2

infixl 7 &*

traverseP :: Parser a b -> Parser [a] [b]
traverseP (Parser p) = Parser $ traverse p

(&**) :: Parser a [b] -> Parser b c -> Parser a [c]
p1 &** p2 = p1 &* traverseP p2

infixl 7 &**

(&=) :: Parser a a' -> Parser b b' -> Parser (a, b) (a', b')
Parser pa &= Parser pb = Parser (\(a, b) -> liftA2 (,) (pa a) (pb b))

infixl 6 &=

(&+) :: Show a => Parser a b -> Parser a c -> Parser [a] (b, c)
pa &+ pb = pairP &* (pa &= pb)

infixl 6 &+

singleP :: Show a => Parser [a] a
singleP =
  Parser $ \case
    [x] -> Right x
    other -> Left $ "singleP: expected 1 element, got " ++ show other

pairP :: Show a => Parser [a] (a, a)
pairP =
  Parser $ \case
    [x, y] -> Right (x, y)
    other -> Left $ "pairP: expected 2 elements, got " ++ show other

pairPWith :: Show a => (b -> c -> d) -> Parser a b -> Parser a c -> Parser [a] d
pairPWith f pb pc = uncurry f <$> pb &+ pc
