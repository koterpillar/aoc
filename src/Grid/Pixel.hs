module Grid.Pixel
  ( Color(..)
  , colorWhite
  , colorBlack
  , colorLightGray
  , colorDarkGray
  , Pixel(..)
  , bgPixel
  , fgPixel
  , defaultPixel
  , kittyDisplay
  , LazyByteString
  , PixelDisplayOptions
  , pixelZoom
  , pixelCheckerOffset
  , pixelCheckerSize
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as ByteString
import qualified Data.ByteString.Base64  as Base64
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as Lazy

import           Data.Word               (Word8)

import           Utils

type LazyByteString = Lazy.ByteString

type Color = (Word8, Word8, Word8)

colorWhite :: Color
colorWhite = (255, 255, 255)

colorBlack :: Color
colorBlack = (0, 0, 0)

colorLightGray :: Color
colorLightGray = (200, 200, 200)

colorDarkGray :: Color
colorDarkGray = (50, 50, 50)

colorMap :: (Word8 -> Word8) -> Color -> Color
colorMap fn (r, g, b) = (fn r, fn g, fn b)

colorLighten :: Word8 -> Color -> Color
colorLighten i =
  colorMap $ \x ->
    if x > 255 - i
      then 255
      else x + i

colorDarken :: Word8 -> Color -> Color
colorDarken i =
  colorMap $ \x ->
    if x < i
      then 0
      else x - i

newtype Pixel =
  Pixel Color

bgPixel :: Pixel
bgPixel = Pixel colorWhite

fgPixel :: Pixel
fgPixel = Pixel colorBlack

defaultPixel :: Char -> Pixel
defaultPixel ' ' = bgPixel
defaultPixel '#' = fgPixel
defaultPixel '·' = bgPixel
defaultPixel 'O' = Pixel (255, 0, 0)
defaultPixel 'X' = Pixel (0, 255, 0)
defaultPixel c   = error $ "defaultPixel: " <> show c

data PixelDisplayOptions = PixelDisplayOptions
  { _pixelZoom          :: Maybe Int
  , _pixelCheckerSize   :: Maybe Int
  , _pixelCheckerOffset :: Maybe Int
  } deriving (Show)

instance Semigroup PixelDisplayOptions where
  p1 <> p2 =
    p1
      { _pixelZoom = _pixelZoom p1 <|> _pixelZoom p2
      , _pixelCheckerSize = _pixelCheckerSize p1 <|> _pixelCheckerSize p2
      , _pixelCheckerOffset = _pixelCheckerOffset p1 <|> _pixelCheckerOffset p2
      }

instance Monoid PixelDisplayOptions where
  mempty = PixelDisplayOptions Nothing Nothing Nothing

pixelZoom :: Int -> PixelDisplayOptions
pixelZoom z = mempty {_pixelZoom = Just z}

pixelCheckerSize :: Int -> PixelDisplayOptions
pixelCheckerSize z = mempty {_pixelCheckerSize = Just z}

pixelCheckerOffset :: Int -> PixelDisplayOptions
pixelCheckerOffset z = mempty {_pixelCheckerOffset = Just z}

-- https://sw.kovidgoyal.net/kitty/graphics-protocol/#rgb-and-rgba-data
kittyDisplay :: PixelDisplayOptions -> [[Pixel]] -> LazyByteString
kittyDisplay PixelDisplayOptions {..} ps =
  Builder.toLazyByteString $ go True pixels <> "\n"
  where
    imgEscape :: [(Char, Builder)] -> ByteString -> Builder
    imgEscape attrs payload =
      esc
        <> "_G"
        <> intercalateM "," [Builder.char8 k <> "=" <> v | (k, v) <- attrs]
        <> ";"
        <> Builder.byteString payload
        <> esc
        <> "\\"
    go _ "" = ""
    go isFirst payload =
      imgEscape
        (attrs isFirst
           <> [ ( 'm'
                , Builder.intDec
                    $ if ByteString.null rest
                        then 0
                        else 1)
              ])
        chunk
        <> go False rest
      where
        (chunk, rest) = ByteString.splitAt 1024 payload
        attrs False = []
        attrs True =
          [ ('a', "T")
          , ('f', "24")
          , ('s', Builder.intDec width)
          , ('v', Builder.intDec height)
          ]
    width = zoom * length (headE "kittyDisplay: empty data" ps)
    height = zoom * length ps
    zoomMap :: Monoid b => (Int -> a -> b) -> [a] -> b
    zoomMap fn = foldMap (uncurry fn) . concatMap (replicate zoom) . zipN 0
    pixels =
      Base64.encode
        $ Lazy.toStrict
        $ Builder.toLazyByteString
        $ zoomMap (zoomMap . pixelData) ps
    pixelData x y (Pixel c) =
      let (r, g, b) = tint x y c
       in foldMap Builder.word8 [r, g, b]
    zoom = fromMaybe 10 _pixelZoom
    sz = fromMaybe 0 _pixelCheckerSize
    offset = fromMaybe 0 _pixelCheckerOffset
    tint :: Int -> Int -> Color -> Color
    tint x y
      | sz == 0 = id
      | cell x == cell y = colorLighten amount
      | otherwise = colorDarken amount
      where
        cell a = even $ (a + offset) `div` sz
        amount = 25

esc :: Builder
esc = Builder.int8 27
