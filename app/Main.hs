module Main where

import System.Random
import System.Environment

import PolygonArt (randomizedPolygonArt, polygon, Vertex(Vertex))
import SvgDrawing (drawPolygon, drawSvg)

main :: IO ()
main = do
  -- width and height of the image
  let size = 1000.0 :: Double
  -- starting polygon: A rectangle
  let rectangle = polygon [Vertex 0 0, Vertex size 0, Vertex size size, Vertex 0 size]
  let poly = rectangle

  --let art = regularPolygonArt 8 1 1 poly
  --generator <- getStdGen
  
  -- retrieve the RNG seed from the command line.
  args <- getArgs
  let seed = case args of
        [arg] -> read arg :: Int
        _ -> 10
  let generator = mkStdGen seed

  -- generate the image data
  let art = randomizedPolygonArt generator 6 poly
  -- generate the svg output
  let picture = drawSvg size size (map drawPolygon art)

  putStrLn picture
