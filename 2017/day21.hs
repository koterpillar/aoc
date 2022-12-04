{-# LANGUAGE RankNTypes #-}

import           Control.Arrow
import           Control.Lens

import           Data.List
import           Data.List.Extra

import           Data.Either

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.Ord

import           Data.Maybe

import           Data.Set           (Set)
import qualified Data.Set           as Set

import           Text.Parsec
import           Text.Parsec.Number
import           Text.Parsec.String

import           Utils

chunksOf23 :: [a] -> [[a]]
chunksOf23 lst
  | length lst `mod` 2 == 0 = chunksOf 2 lst
  | otherwise = chunksOf 3 lst

chunked2' :: (forall b. [b] -> [[b]]) -> Iso' [[a]] [[[[a]]]]
chunked2' chunking =
  iso
    (map (transpose . map chunking) . chunking)
    (concatMap (map concat . transpose))

chunked2 :: Int -> Iso' [[a]] [[[[a]]]]
chunked2 n = chunked2' $ chunksOf n

chunked2_23 :: Iso' [[a]] [[[[a]]]]
chunked2_23 = chunked2' chunksOf23

newtype Pattern =
  Pattern
    { pItems :: [[Bool]]
    }
  deriving (Eq, Ord)

pIso :: Iso' Pattern [[Bool]]
pIso = iso pItems Pattern

iIso :: Iso' Bool Char
iIso = iso b c
  where
    b True  = '#'
    b False = '.'
    c '#' = True
    c _   = False

instance Show Pattern where
  show p = p ^. pIso . mapping (mapping iIso) . to unlines

pSize :: Pattern -> Int
pSize (Pattern p) = length p

newtype Grid =
  Grid
    { gGrid :: [[Bool]]
    }

gIso :: Iso' Grid [[Bool]]
gIso = iso gGrid Grid

instance Show Grid where
  show g = g ^. gIso . mapping (mapping iIso) . to unlines

gSize :: Grid -> Int
gSize (Grid g) = length g

gPatternSize :: Grid -> Int
gPatternSize g =
  let gs = gSize g
   in if gs `mod` 2 == 0
        then 2
        else 3

gPatterns :: Iso' Grid [[Pattern]]
gPatterns = gIso . chunked2_23 . mapping (mapping (from pIso))

data Rule =
  Rule
    { rFrom :: Pattern
    , rTo   :: Pattern
    }

parseRule :: Parser Rule
parseRule = Rule <$> parsePattern <*> (string " => " *> parsePattern)

parsePattern :: Parser Pattern
parsePattern = Pattern <$> sepBy (many elt) (char '/')
  where
    elt = (char '.' *> pure False) <|> (char '#' *> pure True)

pFlipX :: Pattern -> Pattern
pFlipX (Pattern x) = Pattern (map reverse x)

pFlipY :: Pattern -> Pattern
pFlipY (Pattern x) = Pattern (reverse x)

pTranspose :: Pattern -> Pattern
pTranspose (Pattern x) = Pattern (transpose x)

pCanonical :: Pattern -> Pattern
pCanonical p =
  minimum $ do
    f1 <- [pFlipX, id]
    f2 <- [pFlipY, id]
    f3 <- [pTranspose, id]
    pure $ (f1 . f2 . f3) p

type Rules = Map Pattern Pattern

pApply :: Rules -> Pattern -> Pattern
pApply rules p =
  fromMaybe (error $ "Pattern " ++ show p ++ " not found in rules") $
  Map.lookup (pCanonical p) rules

gApply :: Rules -> Grid -> Grid
gApply rules g = g ^. gPatterns . to (map $ map $ pApply rules) . from gPatterns

initial :: Grid
initial = Grid $ map (map (== '#')) [".#.", "..#", "###"]

toRules = Map.fromList . map (pCanonical . rFrom &&& rTo)

exampleRules :: Rules
exampleRules =
  toRules $
  map
    (justParse parseRule)
    ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]

gTotalDots :: Grid -> Int
gTotalDots =
  sum .
  map
    (sum .
     map
       (\x ->
          if x
            then 1
            else 0)) .
  gGrid
