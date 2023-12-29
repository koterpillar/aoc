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

-- https://sw.kovidgoyal.net/kitty/graphics-protocol/#rgb-and-rgba-data
kittyDisplay :: Int -> [[Pixel]] -> LazyByteString
kittyDisplay zf ps = Builder.toLazyByteString $ go True pixels
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
    width = zf * length (headE "kittyDisplay: empty data" ps)
    height = zf * length ps
    zoomMap :: Monoid b => (a -> b) -> [a] -> b
    zoomMap fn = foldMap fn . concatMap (replicate zf)
    pixels =
      Base64.encode
        $ Lazy.toStrict
        $ Builder.toLazyByteString
        $ zoomMap (zoomMap pixelData) ps
    pixelData (Pixel (r, g, b)) = foldMap Builder.word8 [r, g, b]

esc :: Builder
esc = Builder.int8 27
