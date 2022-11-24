module Y2017.Knot where

import           Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text

import           Numeric

import           Utils

data Knot =
  Knot
    { kItems    :: !(Map Int Int) -- ^ map positions to numbers
    , kPos      :: !Int
    , kSkipSize :: !Int
    }

kAt :: Knot -> Int -> Int
kAt k i =
  let (Just v) = mapLookup i (kItems k)
   in v

kLength = length . kItems

mkKnot :: Int -> Knot
mkKnot len =
  Knot
    { kItems = mapFromList $ zip [0 .. len - 1] [0 .. len - 1]
    , kPos = 0
    , kSkipSize = 0
    }

instance Show Knot where
  show k =
    "Knot {" ++
    unwords (map showKItem [0 .. kLength k - 1]) ++
    ", kSkipSize = " ++ show (kSkipSize k) ++ "}"
    where
      showKItem i
        | i == kPos k = "[" ++ show (kAt k i) ++ "]"
        | otherwise = show (kAt k i)

kNorm :: Knot -> Int -> Int
kNorm knot a = a `mod` kLength knot

kAdd :: Knot -> Int -> Int -> Int
kAdd knot a b = kNorm knot $ a + b

kSub :: Knot -> Int -> Int -> Int
kSub knot a b = kNorm knot $ a - b

kSkip :: Int -> Knot -> Knot
kSkip size knot = knot {kPos = kAdd knot (kPos knot) size}

kIncSkip :: Knot -> Knot
kIncSkip k = k {kSkipSize = kSkipSize k + 1}

kRotate :: Int -> Knot -> Knot
kRotate len knot = knot {kItems = Map.mapKeys rotKey $ kItems knot}
  where
    rotKey i
      | i - pos < len = pos + pos + len - i - 1
      | otherwise = i
    pos = kPos knot
    (+) = kAdd knot
    (-) = kSub knot

kStep :: Int -> Knot -> Knot
kStep len knot = kIncSkip $ kSkip (kSkipSize knot + len) $ kRotate len knot
  where
    (+) = kAdd knot

kSteps :: [Int] -> Knot -> Knot
kSteps lens knot = foldr kStep knot $ reverse lens

strToLens :: Text -> [Int]
strToLens str =
  join $ replicate 64 $ map ord (Text.unpack str) ++ [17, 31, 73, 47, 23]

kHash :: Text -> Knot -> Knot
kHash str = kSteps $ strToLens str

kDense :: Knot -> [Int]
kDense = map (foldr1 xor) . chunksOf 16 . toList . kItems

kShowDense :: [Int] -> Text
kShowDense =
  mconcat . map (Text.justifyRight 2 '0' . Text.pack . flip showHex "")

knotHash :: Text -> Text
knotHash str = kShowDense $ kDense $ kHash str $ mkKnot 256
