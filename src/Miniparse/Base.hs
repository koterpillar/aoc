module Miniparse.Base where

import           Control.Applicative

import qualified Data.Aeson          as Aeson

import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text

import           Bit
import           Grid
import           Utils

newtype Parser src dest =
  Parser
    { runParse :: src -> Either String dest
    }

instance Functor (Parser src) where
  fmap f (Parser p) = Parser $ fmap f . p

pureP :: (src -> dest) -> Parser src dest
pureP = Parser . (pure .)

idP :: Parser src src
idP = pureP id

constP :: dest -> Parser src dest
constP = Parser . const . Right

failP :: String -> Parser src dest
failP = Parser . const . Left

justParse :: Parser src dest -> src -> dest
justParse parser src =
  case runParse parser src of
    Left err   -> error err
    Right dest -> dest

choiceP :: (Ord src, Show src) => [(src, dest)] -> Parser src dest
choiceP choices =
  let cmap = mapFromList choices
   in Parser $ \src ->
        case mapLookup src cmap of
          Just dest -> Right dest
          Nothing ->
            Left $
            "Unexpected: " ++
            show src ++
            ", expected one of: " ++ intercalate "," (map (show . fst) choices)

choiceEBP ::
     (Ord src, Show src, Bounded dest, Enum dest) => [src] -> Parser src dest
choiceEBP = choiceP . (`zip` enumerate)

requireP :: (Show a, Eq a) => a -> Parser a ()
requireP expected =
  Parser $ \case
    actual
      | actual == expected -> Right ()
      | otherwise ->
        Left $ "Expected " ++ show expected ++ ", got " ++ show actual

linesP :: Parser Text [Text]
linesP = pureP Text.lines

wordsP :: Parser Text [Text]
wordsP = pureP Text.words

splitP :: Eq a => [a] -> Parser [a] [[a]]
splitP sep = pureP $ splitOn sep

lineGroupsP :: Parser Text [[Text]]
lineGroupsP = linesP &* splitP [""]

unconsP :: Parser [a] (a, [a])
unconsP =
  Parser $ \case
    (x:xs) -> Right (x, xs)
    []     -> Left "unconsP: unexpected empty list"

tsplitP :: Text -> Parser Text [Text]
tsplitP sep = pureP $ Text.splitOn sep

tspanP :: (Char -> Bool) -> Parser Text (Text, Text)
tspanP = pureP . Text.span

readEitherErr :: Read a => String -> Either String a
readEitherErr t =
  case readEither t of
    Left err -> Left $ "readP: " ++ err ++ "; input: " ++ show t
    Right x  -> Right x

readP :: Read a => Parser Text a
readP = stringP &* Parser readEitherErr

integerP :: Parser Text Int
integerP = readP

integersP :: Text -> Parser Text [Int]
integersP sep = tsplitP sep &** integerP

integersSpaceP :: Parser Text [Int]
integersSpaceP = wordsP &** integerP

charactersP :: Parser Text [Char]
charactersP = pureP Text.unpack

charP :: Parser Text Char
charP = charactersP &* singleP

stringP :: Parser Text String
stringP = charactersP

digitsP :: Parser Text [Int]
digitsP = charactersP &** (pureP Text.singleton &* integerP)

position2P :: Parser Text Position2
position2P = tsplitP "," &* ap2P Position2 integerP integerP

charGridMaybeP :: Parser Char (Maybe item) -> Parser Text (Grid2 item)
charGridMaybeP elemP =
  Map.mapMaybe id . fromMatrixG <$> linesP &** charactersP &** elemP

charGridP' :: Parser Char item -> Parser Text (Grid2 item)
charGridP' elemP =
  charGridMaybeP $ (Just <$> elemP) &| (Nothing <$ requireP '.')

gridItemP ::
     (Bounded item, Enum item, GridItem item, Show item) => Parser Char item
gridItemP = choiceP $ map (showInGrid &&& id) enumerate

charGridP ::
     (Bounded item, Enum item, GridItem item, Show item)
  => Parser Text (Grid2 item)
charGridP = charGridP' gridItemP

digitGridP :: Parser Text (Grid2 Int)
digitGridP = charGridP' $ pureP Text.singleton &* integerP

dotGridP :: Parser Text (Grid2 ())
dotGridP = charGridMaybeP $ boolToMaybe <$> dotP
  where
    boolToMaybe True  = Just ()
    boolToMaybe False = Nothing

dotP :: Parser Char Bool
dotP = choiceEBP ".#"

dotsP :: Parser Text [Bool]
dotsP = charactersP &** dotP

bitsP :: Parser Text BitString
bitsP = charactersP &** choiceEBP ['0', '1']

jsonP :: Aeson.FromJSON a => Parser Text a
jsonP = pureP Text.encodeUtf8 &* Parser Aeson.eitherDecodeStrict'

(&*) :: Parser a b -> Parser b c -> Parser a c
Parser p1 &* Parser p2 = Parser $ p1 >=> p2

infixr 7 &*

traverseP :: Parser a b -> Parser [a] [b]
traverseP (Parser p) = Parser $ traverse p

(&**) :: Parser a [b] -> Parser b c -> Parser a [c]
p1 &** p2 = p1 &* traverseP p2

infixr 7 &**

(&=) :: Parser a a' -> Parser b b' -> Parser (a, b) (a', b')
Parser pa &= Parser pb = Parser (\(a, b) -> liftA2 (,) (pa a) (pb b))

infixl 8 &=

bindP :: Parser a c -> (c -> Parser b d) -> Parser (a, b) d
bindP pa f =
  Parser $ \(a, b) -> do
    c <- runParse pa a
    runParse (f c) b

(&=>) :: Parser a c -> (c -> Parser b d) -> Parser (a, b) d
pa &=> f = bindP pa f

tupleBindP :: (a -> Parser b c) -> Parser (a, b) c
tupleBindP = bindP idP

unconsBindP :: (src -> Parser [src] dest) -> Parser [src] dest
unconsBindP rest = unconsP &* (idP &=> rest)

(&+) :: Show a => Parser a b -> Parser a c -> Parser [a] (b, c)
pa &+ pb = pairP &* (pa &= pb)

infixl 8 &+

(&|) :: Parser a b -> Parser a b -> Parser a b
pa &| pb = Parser $ \a -> runParse pa a <|> runParse pb a

singleP :: Show a => Parser [a] a
singleP =
  Parser $ \case
    [x]   -> Right x
    other -> Left $ "singleP: expected 1 element, got " ++ show other

pairP :: Show a => Parser [a] (a, a)
pairP =
  Parser $ \case
    [x, y] -> Right (x, y)
    other  -> Left $ "pairP: expected 2 elements, got " ++ show other

ap0P :: Show src => b -> Parser [src] b
ap0P f = f <$ filterP null

ap1P :: Show src => (b -> c) -> Parser src b -> Parser [src] c
ap1P f p = f <$> singleP &* p

ap2P ::
     Show src => (b -> c -> d) -> Parser src b -> Parser src c -> Parser [src] d
ap2P f pb pc = uncurry f <$> pb &+ pc

ap3P ::
     Show src
  => (b -> c -> d -> e)
  -> Parser src b
  -> Parser src c
  -> Parser src d
  -> Parser [src] e
ap3P f pb pc pd = unconsP &* (pb &=> \b -> ap2P (f b) pc pd)

ap4P ::
     Show src
  => (b -> c -> d -> e -> f)
  -> Parser src b
  -> Parser src c
  -> Parser src d
  -> Parser src e
  -> Parser [src] f
ap4P f pb pc pd pe = unconsP &* (pb &=> \b -> ap3P (f b) pc pd pe)

ap5P ::
     Show src
  => (b -> c -> d -> e -> f -> g)
  -> Parser src b
  -> Parser src c
  -> Parser src d
  -> Parser src e
  -> Parser src f
  -> Parser [src] g
ap5P f pb pc pd pe pf = unconsP &* (pb &=> \b -> ap4P (f b) pc pd pe pf)

filterP :: Show a => (a -> Bool) -> Parser a a
filterP f =
  Parser $ \a ->
    if f a
      then Right a
      else Left $ "filterP: failed, got " ++ show a

lookupP :: (Ord k, Show k) => k -> Parser (Map k v) v
lookupP k =
  Parser $ \m ->
    case mapLookup k m of
      Nothing ->
        Left $ "lookupP: " ++ show k ++ " not found, got " ++ show (Map.keys m)
      Just v -> Right v
