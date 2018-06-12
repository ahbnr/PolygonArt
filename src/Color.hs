module Color where

import Data.Fixed (mod')

-- (HSL hue sat light) represents a color in HSL format
-- (https://en.wikipedia.org/wiki/HSL_and_HSV)
-- hue in [0;360]
-- sat in [0;1]
-- light in [0;1]
data HSL = HSL Float Float Float

-- Representation of colors in RGB format.
-- with r,g,b in [0;1]
data RGB = RGB Float Float Float
-- Also RGB color format, but meant to be a 3 Byte integer representation
-- such that r,g,b in {0..255} 
data RGB3Byte = RGB3Byte Int Int Int

-- Datatype for color with support for HSL and RGB color formats
data Color = ColorHSL HSL | ColorRGB RGB | ColorRGB3B RGB3Byte

hslToRgb :: HSL -> RGB
-- ^converts a color in HSL format to RGB format
-- based on https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSL
hslToRgb (HSL h s l) = RGB (r1 + m) (g1 + m) (b1 + m)
  where
    c = (1 - abs (2*l - 1)) * s
    h' = h / 60
    x = c * (1 - abs (h' `mod'` 2 - 1))
    (r1, g1, b1)
      | 0 <= h' && h' <= 1 = (c, x, 0)
      | 1 < h' && h' <= 2 = (x, c, 0)
      | 2 < h' && h' <= 3 = (0, c, x)
      | 3 < h' && h' <= 4 = (0, x, c)
      | 4 < h' && h' <= 5 = (x, 0, c)
      | 5 < h' && h' < 6 = (c, 0, x)
      | otherwise       = (0, 0, 0)
    m = l - c/2

rgbTo3Byte :: RGB -> RGB3Byte
-- ^converts a RGB color to it's 3 Byte representation 
rgbTo3Byte (RGB r g b) = RGB3Byte (toByte r) (toByte g) (toByte b)
  where
    toByte = round . (* 255)

complement :: HSL -> HSL
-- ^computes a HSL color's complement
complement (HSL h s l) = HSL ((h + 180) `mod'` 360) s l
