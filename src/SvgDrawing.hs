module SvgDrawing where

import PolygonArt (Polygon, Vertex(Vertex), vertices)

drawSvg ::
       Double    -- ^width of the resulting svg image
    -> Double    -- ^height of the resulting svg image
    -> [String]  -- ^contents (will be separted by line)
    -> String
-- ^wraps the given strings in a \<svg ...>\</svg> tag
drawSvg w h children = concat
  [
      "<svg width=\"",
          show w,
          "\" height=\"",
          show h,
          "\" style=\"stroke-width: 0px; background-color: blue;\">\n",
        unlines children,
      "</svg>"
    ]

drawPolygon :: Polygon -> String
-- ^builds a svg polygon tag
drawPolygon poly = concat ["<polygon points=\"", pointStr, "\" style=\"fill:white;stroke:black;stroke-width:1\"/>"]
  where
    pointStr :: String
    pointStr =
      unwords
        (map (\(Vertex x y) -> concat [show x, ",", show y]) (vertices poly))
