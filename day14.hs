import Data.Char
import Data.Foldable

import Numeric

import Knot
import Utils

showBin :: Int -> String
showBin n = showIntAtBase 2 (head . show) n ""

knotHashBin :: String -> String
knotHashBin = concatMap hexToBin . knotHash

hexToBin :: Char -> String
hexToBin = pad' '0' 4 . showBin . fst . head . readHex . (:[])

grid :: String -> [[Bool]]
grid seed = map (\i -> map binToEmpty $ knotHashBin $ seed ++ "-" ++ show i) [0..127]

binToEmpty '0' = False
binToEmpty '1' = True

showGrid :: [[Bool]] -> IO ()
showGrid = traverse_ $ putStrLn . map gridChar
  where
    gridChar True = '#'
    gridChar False = '.'

gridTopCorner :: Int -> [[a]] -> [[a]]
gridTopCorner n = take n . map (take n)

gridCount :: [[Bool]] -> Int
gridCount = sum . map (sum . map (\i -> if i then 1 else 0))
