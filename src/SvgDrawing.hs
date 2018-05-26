module SvgDrawing where

import PolygonArt (Polygon, Point(Point), points)

drawSvg :: Double -> Double -> [String] -> String
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
drawPolygon poly = concat ["<polygon points=\"", pointStr, "\" style=\"fill:white;stroke:black;stroke-width:1\"/>"]
  where
    pointStr :: String
    pointStr =
      unwords
        (map (\(Point x y) -> concat [show x, ",", show y]) (points poly))
