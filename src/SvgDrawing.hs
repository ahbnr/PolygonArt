module SvgDrawing where

import PolygonArt (ColoredPolygon(ColoredPolygon), Polygon, Vertex(Vertex), vertices)
import Color

drawSvg ::
       Double    -- ^width of the resulting svg image
    -> Double    -- ^height of the resulting svg image
    -> [String]  -- ^contents (will be separted by line)
    -> String
-- ^wraps the given strings in a \<svg ...>\</svg> tag
drawSvg w h children = concat
  [
      "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n", 
      "<svg ",
          "xmlns=\"http://www.w3.org/2000/svg\" ",
          "version=\"1.1\" ",
          "width=\"",
          show w,
          "\" height=\"",
          show h,
          "\" style=\"stroke-width: 0px; background-color: blue;\">\n",
        unlines children,
      "</svg>"
    ]

drawPolygon :: ColoredPolygon -> String
-- ^builds a svg polygon tag
drawPolygon (ColoredPolygon poly color) = concat ["<polygon points=\"", pointStr, "\" style=\"fill:", renderColor color ,";stroke:black;stroke-width:1\"/>"]
  where
    pointStr :: String
    pointStr =
      unwords
        (map (\(Vertex x y) -> concat [show x, ",", show y]) (vertices poly))

renderColor :: Color -> String
-- ^converts a color into SVG/CSS rgb(...) string representation
renderColor (ColorHSL hsl) = (renderColor . ColorRGB . hslToRgb) hsl
renderColor (ColorRGB rgb) = (renderColor . ColorRGB3B . rgbTo3Byte) rgb
renderColor (ColorRGB3B (RGB3Byte r g b)) = concat ["rgb(", show r, ", ", show g, ", ", show b, ")"]
