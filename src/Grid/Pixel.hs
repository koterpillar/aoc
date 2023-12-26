module Grid.Pixel
  ( Color(..)
  , colorWhite
  , colorBlack
  , colorLightGray
  , colorDarkGray
  , Pixel(..)
  , chequeredPixel
  , bgPixel
  , fgPixel
  , defaultPixel
  , kittyDisplay
  ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base64 as Base64

import qualified Data.Text.Encoding     as Text

import           Data.Word              (Word8)

import           Utils

type Color = (Word8, Word8, Word8)

colorWhite :: Color
colorWhite = (255, 255, 255)

colorBlack :: Color
colorBlack = (0, 0, 0)

colorLightGray :: Color
colorLightGray = (200, 200, 200)

colorDarkGray :: Color
colorDarkGray = (50, 50, 50)

type T2 a = (a, a)

newtype Pixel =
  Pixel (T2 (T2 Color))

chequeredPixel :: Color -> Color -> Pixel
chequeredPixel c1 c2 = Pixel ((c1, c2), (c2, c1))

bgPixel :: Pixel
bgPixel = chequeredPixel colorWhite colorLightGray

fgPixel :: Pixel
fgPixel = chequeredPixel colorBlack colorDarkGray

defaultPixel :: Char -> Pixel
defaultPixel ' ' = bgPixel
defaultPixel '#' = fgPixel
defaultPixel '·' = bgPixel
defaultPixel 'O' =
  let red = (255, 0, 0)
   in Pixel ((red, red), (red, red))
defaultPixel 'X' =
  let green = (0, 255, 0)
   in Pixel ((green, green), (green, green))
defaultPixel c = error $ "defaultPixel: " <> show c

-- https://sw.kovidgoyal.net/kitty/graphics-protocol/#rgb-and-rgba-data
kittyDisplay :: Int -> [[Pixel]] -> ByteString
kittyDisplay zf ps = go True pixels
  where
    imgEscape :: [(ByteString, ByteString)] -> ByteString -> ByteString
    imgEscape attrs payload =
      esc
        <> "_G"
        <> ByteString.intercalate "," [k <> "=" <> v | (k, v) <- attrs]
        <> ";"
        <> payload
        <> esc
        <> "\\"
    go _ "" = ""
    go isFirst payload =
      imgEscape
        (attrs isFirst
           <> [ ( "m"
                , if ByteString.null rest
                    then "0"
                    else "1")
              ])
        chunk
        <> go False rest
      where
        (chunk, rest) = ByteString.splitAt 1024 payload
        attrs False = []
        attrs True =
          [("a", "T"), ("f", "24"), ("s", bshow width), ("v", bshow height)]
    width = length selectors * length (headE "kittyDisplay: empty data" ps)
    height = length selectors * length ps
    selectors = [fst, snd]
    pixels =
      Base64.encode
        $ ByteString.pack
            [ c
            | line <- ps
            , selector1 <- selectors
            , _ <- [1 .. zf]
            , Pixel els <- line
            , selector2 <- selectors
            , let (r, g, b) = selector2 (selector1 els)
            , _ <- [1 .. zf]
            , c <- [r, g, b]
            ]

bshow :: Show a => a -> ByteString
bshow = Text.encodeUtf8 . tshow

esc :: ByteString
esc = ByteString.singleton 27
