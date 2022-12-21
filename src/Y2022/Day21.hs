module Y2022.Day21 where

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import           AOC
import           Utils

data Expr
  = Const Int
  | Op Char Text Text
  | Input
  deriving (Ord, Eq, Show)

exprP :: Parser Text Expr
exprP = wordsP &* ap1P Const integerP &| ap3P (flip Op) idP charP idP

type Exprs = Map Text Expr

parser :: Parser Text Exprs
parser = Map.fromList <$> linesP &** tsplitP ": " &* idP &+ exprP

evaluate :: Text -> Exprs -> Int
evaluate k m = ev1 $ mapLookupE "evaluate" k m
  where
    ev1 (Const a)  = a
    ev1 (Op o a b) = op o (evaluate a m) (evaluate b m)

op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = div

part1 :: Exprs -> Int
part1 = evaluate "root"

fixup = Map.adjust aroot "root" . Map.insert "humn" Input
  where
    aroot (Op _ a b) = Op '-' a b

newtype Expr2 =
  Powers [Double]
  deriving (Ord, Eq)

mkPowers :: [Double] -> Expr2
mkPowers = Powers . dropWhileEnd (== 0)

instance Show Expr2 where
  show (Powers p) = intercalate " + " $ map s1 $ zipN 0 $ mkzero p
    where
      mkzero [] = [0]
      mkzero x  = x
      s1 (0, n) = show n
      s1 (1, n) = show n ++ "x"
      s1 (p, n) = show n ++ "x^" ++ show p

type Expr2s = Map Text Expr2

convert :: Exprs -> Expr2s
convert m = Map.map go m
  where
    go1 k = go $ mapLookupE "convert" k m
    go (Const i)  = mkPowers [fromIntegral i]
    go (Op o a b) = op2 o (go1 a) (go1 b)
    go Input      = mkPowers [0, 1]

op2 '+' (Powers a) (Powers b)   = mkPowers $ zipWithLongest (onZero (+)) a b
op2 '-' (Powers a) (Powers b)   = mkPowers $ zipWithLongest (onZero (-)) a b
op2 '*' (Powers a) (Powers b)   = mulPowers a b
op2 '/' (Powers a) (Powers [c]) = mkPowers $ map (/ c) a
op2 '/' _ b                     = error $ "op2 / " ++ show b

mulPowers :: [Double] -> [Double] -> Expr2
mulPowers a b = mkPowers $ [mapLookupE "mulPowers" k m | k <- ks]
  where
    m = mapFromListSum [(i + j, x * y) | (i, x) <- zipN 0 a, (j, y) <- zipN 0 b]
    maxPower = fst $ Map.findMax m
    ks = [0 .. maxPower]

onZero :: Num a => (a -> a -> a) -> Maybe a -> Maybe a -> a
onZero f = f `on` fromMaybe 0

-- ax + b = 0
-- x = -b / a
solve :: Expr2 -> Double
solve (Powers [b, a]) = negate $ b / a

part2 :: Exprs -> Int
part2 = round . solve . mapLookupE "part2" "root" . convert . fixup

tasks = Tasks 2022 21 (CodeBlock 0) parser [Task part1 152, Task part2 301]
