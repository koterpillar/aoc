module Y2023.Day15
  ( tasks
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

parser = pureP Text.stripEnd &* tsplitP "," &** charactersP

calcHash :: String -> Int
calcHash = go 0
  where
    go r []     = r
    go r (c:cs) = flip go cs $ ((r + ord c) * 17) `mod` 256

part1 = sum . map calcHash

data LensOp
  = LRemove String
  | LAdd String Int
  deriving (Show, Eq, Ord)

lensOpsP :: Parser [String] [LensOp]
lensOpsP =
  traverseP
    $ pureP Text.pack
        &* ((tsplitP "-" &* ap2P (\x _ -> LRemove x) charactersP (requireP ""))
              &| (tsplitP "=" &* ap2P LAdd charactersP integerP))

type Box = [(String, Int)]

type Lenses = Map Int Box

focusingPowers :: Lenses -> Int
focusingPowers m =
  sum $ do
    (k, ls) <- Map.toList m
    (i, (_, l)) <- zip [1 ..] ls
    pure $ succ k * i * l

setupLenses :: [LensOp] -> Lenses
setupLenses = foldl' (flip setupLens) Map.empty

setupLens :: LensOp -> Lenses -> Lenses
setupLens (LRemove x) m = Map.insert k (filter (\(l, _) -> l /= x) e) m
  where
    k = calcHash x
    e = fromMaybe [] $ Map.lookup k m
setupLens (LAdd x v) m = Map.insert k e' m
  where
    k = calcHash x
    e = fromMaybe [] $ Map.lookup k m
    exists = any (\(l, _) -> l == x) e
    e' =
      if exists
        then map
               (\(l, v0) ->
                  if l == x
                    then (l, v)
                    else (l, v0))
               e
        else e ++ [(x, v)]

part2 = focusingPowers . setupLenses . justParse lensOpsP

tasks = Tasks 2023 15 (CodeBlock 0) parser [Task part1 1320, Task part2 145]
