module Main where

import System.Random
import System.Environment

import PolygonArt (randomizedPolygonArt, polygon, Point(Point))
import SvgDrawing (drawPolygon, drawSvg)

main :: IO ()
main = do
  let size = 1000.0 :: Double
  let rectangle = polygon [Point 0 0, Point size 0, Point size size, Point 0 size]
  let poly = rectangle

  --let art = regularPolygonArt 8 1 1 poly
  --generator <- getStdGen
  args <- getArgs
  let seed = case args of
        [arg] -> read arg :: Int
        _ -> 10
  let generator = mkStdGen seed
  let art = randomizedPolygonArt generator 7 poly
  let picture = drawSvg size size (map drawPolygon art)
  putStrLn picture
  --putStrLn $ map (\_ -> ' ') picture
