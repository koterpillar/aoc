{-# LANGUAGE RankNTypes #-}
import Control.Arrow
import Control.Lens

import Data.List
import Data.List.Extra

import Data.Either

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ord

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.Number
import Text.Parsec.String

import Utils

chunked :: Int -> Iso' [a] [[a]]
chunked n = iso (chunksOf n) concat

chunked2 :: Int -> Iso' [[a]] [[[[a]]]]
chunked2 n =
  iso
    (map (transpose . map (chunksOf n)) . chunksOf n)
    (concatMap (map concat . transpose))

newtype Pattern = Pattern
  { pItems :: [[Bool]]
  } deriving (Eq, Ord)

pIso :: Iso' Pattern [[Bool]]
pIso = iso pItems Pattern

iIso :: Iso' Bool Char
iIso = iso b c
  where
    b True = '#'
    b False = '.'
    c '#' = True
    c _ = False

instance Show Pattern where
  show p = p ^. pIso . mapping (mapping iIso) . to unlines

pSize :: Pattern -> Int
pSize (Pattern p) = length p

newtype Grid = Grid
  { gGrid :: [[Pattern]]
  }

gIso :: Iso' Grid [[Pattern]]
gIso = iso gGrid Grid

gBlockSize :: Grid -> Int
gBlockSize = pSize . head . head . gGrid

instance Show Grid where
  show g =
    let gs = gBlockSize g
    in g ^. gIso . mapping (mapping pIso) . from (chunked2 gs) .
       mapping (mapping iIso) .
       to unlines

data Rule = Rule
  { rFrom :: Pattern
  , rTo :: Pattern
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
gApply rules = Grid . fixup4 . map (map (pApply rules)) . gGrid

fixup4 :: [[Pattern]] -> [[Pattern]]
fixup4 ps
  | pSize (head $ head ps) == 3 = ps
  | otherwise =
    ps ^. mapping (mapping pIso) . from (chunked2 4) . chunked2 2 .
    mapping (mapping (from pIso))

initial :: Grid
initial = Grid [[Pattern $ map (map (== '#')) [".#.", "..#", "###"]]]

toRules = Map.fromList . map (pCanonical . rFrom &&& rTo)

exampleRules :: Rules
exampleRules =
  toRules $
  map
    (justParse parseRule)
    ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]

gTotalDots :: Grid -> Int
gTotalDots = sum . map pTotalDots . concat . gGrid

pTotalDots :: Pattern -> Int
pTotalDots =
  sum .
  map
    (sum .
     map
       (\x ->
          if x
            then 1
            else 0)) .
  pItems
