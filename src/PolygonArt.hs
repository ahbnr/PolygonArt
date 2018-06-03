module PolygonArt where

import Data.Array
import Data.List
import Debug.Trace
import System.Random
import Data.Maybe
import Numeric.Natural as Natural

import qualified Statistics

data Vertex = Vertex Double Double deriving (Eq, Show)

-- Polygons are modelled as arrays
--
-- All functions using this type make the following assumptions:
-- * the vertices within the array are in clockwise order around the boundary
-- * indices start at 1 (allows for cleaner code with the Natual data type).
type Polygon = Array Natural Vertex

polygon :: [Vertex] -> Polygon
-- ^construct a polygon from a set of vertices.
polygon ps = listArray (1, length' ps) ps

vertices :: Polygon -> [Vertex]
-- ^get a list of all vertices of a polygon
vertices = elems

nthVertex :: Polygon -> Natural -> Maybe Vertex
-- ^Retrieve the nth vertex of a polygon in clockwise order around the boundary
nthVertex poly i
  | len <= 0 || i < minIndex = Nothing
  | otherwise              = Just (poly ! i')
  where
    -- normalize i: Too high indices are wrapped around
    i' = minIndex + (i - minIndex) `mod` len

    (minIndex, _) = bounds poly
    len = numVertices poly

numVertices :: Polygon -> Natural
-- ^retrieve the number of vertices of a polygon / the number of corners it has
numVertices poly = len (bounds poly)
  where
    len :: (Natural, Natural) -> Natural
    len (minIndex, maxIndex)
      | minIndex <= maxIndex = maxIndex - minIndex + 1
      | otherwise           = 0

polygonArea :: Polygon -> Double
-- ^calculate the signed are of a polygon
-- see also: https://en.wikipedia.org/wiki/Polygon#Area_and_centroid
polygonArea poly = 0.5 * sum [summand (nth i) (nth (i+1)) | i <- [1..n]]
  where
    summand :: Vertex -> Vertex -> Double
    summand (Vertex xi yi) (Vertex xi1 yi1) = xi * yi1 - xi1 * yi

    -- indices are selected to be safe. So we can use unsafeJust.
    -- This could be avoided in the future, by not accessing by index.
    nth = unsafeJust . nthVertex poly
    n = numVertices poly

unsafeJust :: Maybe a -> a
-- ^Unpack a Maybe if it's guaranteed to be a Just value.
--
-- In some rare cases this function is needed to unpack a Maybe
-- expression.
-- Its almost the same as fromJust, but more expressive
unsafeJust x
  | isJust x  = fromJust x
  | otherwise = error "Something went terribly wrong, this should never happen. Reason: Tried to unpack an empty Maybe"

take' :: Integral a => a -> [b] -> [b]
-- ^This is just a wrapper around the original take
-- function to accept all integral values as length.
take' n = take (fromIntegral n)

length' :: (Foldable t, Integral b) => t a -> b
-- ^This is just a wrapper around the original length
-- function to return any integral value as length.
length' = fromIntegral . length

polygonCenter :: Polygon -> Vertex
-- ^Calculate centroid of a polygon
-- See also https://en.wikipedia.org/wiki/Polygon#Area_and_centroid
polygonCenter poly = Vertex cX cY
  where
    a = let a' = polygonArea poly in
      if a' == 0 then
        error "Empty polygon area"
      else
        a'
    cX = (sum [summandX (nth i) (nth (i+1)) | i <- [1..n]]) / (6 * a)
    cY = (sum [summandY (nth i) (nth (i+1)) | i <- [1..n]]) / (6 * a)

    -- indices are selected to be safe. So we can use unsafeJust.
    -- This could be avoided in the future, by not accessing by index.
    nth = unsafeJust . nthVertex poly

    summandX :: Vertex -> Vertex -> Double
    summandX (Vertex xi yi) (Vertex xi1 yi1) = (xi + xi1) * (xi * yi1 - xi1 * yi)

    summandY :: Vertex -> Vertex -> Double
    summandY (Vertex xi yi) (Vertex xi1 yi1) = (yi + yi1) * (xi * yi1 - xi1 * yi)

    n = numVertices poly

splitPolygon ::
     Natural   -- ^iteration step
  -> Natural   -- ^distance between two triangle vertices on the boundary
  -> Polygon
  -> [Polygon]
-- ^splits a polygon P into triangles.
--
-- This function connects pairs of vertices of a polygon with its
-- centroid to form triangles.
--
-- Every `step` vertex will be used to initialize a triangle
-- with the centroid and another vertex `distance` hops away.
splitPolygon step distance poly
  -- dont split zero area polygons
  | a == 0 = []
  -- if no steps shall be performed, only one split will be
  -- performed with the first vertex
  | step' == 0 = [makeSubTriangle (nth 1) (nth distance)]
  | otherwise =
      [makeSubTriangle (nth i) (nth i') |
          i <- [1,1+step'..n],
          let i' = i + distance
        ]
  where
    -- normalize the step to fit within a range
    -- from 0 to |P| - 1
    step' = step `mod` n

    -- select the nth vertex of P from vertex 0
    --
    -- indices are selected to be safe. So we can use unsafeJust.
    -- This could be avoided in the future, by not accessing by index.
    nth = unsafeJust . nthVertex poly

    -- n = |P|
    n = numVertices poly

    a = polygonArea poly
    center = polygonCenter poly

    -- create a triangle from two vertices and the centroid of P
    makeSubTriangle p1 p2 =
      polygon [center, p1, p2]


randomSplitPolygon :: Float -> Float -> Float -> Float -> Polygon -> [Polygon]
-- ^split a polygon P randomly into triangles.
--
-- This function connects random pairs of vertices of a polygon with its
-- centroid to form triangles.
--
-- it may also decide, not to split at all
randomSplitPolygon splitProbability splitRN stepRN distanceRN seed
  | splitRN < splitProbability = splitPolygon step distance seed 
  | otherwise                  = []
  where
    -- randomly selected, weighted step within a range
    -- from 0 to |P| - 1
    step' = selectStep stepRN
    step = step' -- traceShow (step', stepWeights, stepRN) step'

    -- randomly selected, weighted distance within a range
    -- from 0 to |P| - 1
    distance = selectStep distanceRN

    -- n = |P|
    n = numVertices seed

    -- single steps should be most common, a full circle step
    -- the least common (it creates a "line" between the center and th
    -- current vertex)
    --
    -- calculating [1,2,...,n-1,0] this way is not very efficient, but
    -- expressive about the circular structure. So I keep it like this
    -- for now.
    validSteps :: [Natural]
    validSteps = (`mod` n) <$> [1..n]

    selectStep :: Float -> Natural 
    selectStep =
        (fromMaybe 1) -- standard value on empty selection shall be 1
      . (Statistics.weightedSelector (Statistics.cumulativeGeometricDistribution 0.8) validSteps)

repeatedSplits ::
     (Polygon -> [Polygon]) -- ^splitting function
  -> Polygon                -- ^starting polygon
  -> Natural                -- ^splitting depth / number of splittings
  -> [Polygon]
-- ^repeadedly apply a polygon splitting function to a starting polygon
-- 
-- returns the set of resulting polygons after `depth` applications of the split operation.
repeatedSplits f seed depth = 
  foldl
    union
    []
    (take' depth rawSplits)
  where
    rawSplits :: [[Polygon]]
    rawSplits = iterate (concat . map f) [seed]

regularPolygonArt ::
     Natural -- ^number of splitting operations (depth)
  -> Natural -- ^number of vertices after which the next split operation will be applied
  -> Natural -- ^distance between polygon pairs on the boundary when forming a triangle
  -> Polygon
  -> [Polygon]
-- ^split a polygon P into triangles.
--
-- This function connects pairs of vertices of a polygon with its
-- centroid to form triangles.
--
-- The pairs are chosen to be exactly `distance` edges apart.
-- The split operation will be applied every `step` vertices
regularPolygonArt depth step distance seed =
  repeatedSplits (splitPolygon step distance) seed depth

listUnion :: Eq a => [[a]] -> [a]
-- ^calculates the union of all lists within a list
listUnion = foldl union []

randomizedPolygonArt :: RandomGen gen =>
     gen       
  -> Natural    -- ^depth until which splits shall be applied
  -> Polygon    -- ^starting polygon
  -> [Polygon]
-- ^generate a random piece of polygon "art"
randomizedPolygonArt g depth seed = foldl union [] rawSplits
  where
    splitRNS = randoms g
    stepRNS = randoms g
    distanceRNS = randoms g

    combinedRNS :: [(Float, Float, Float)]
    combinedRNS = zip3 splitRNS stepRNS distanceRNS
    
    randomizedSplitters :: [Polygon -> [Polygon]]
    randomizedSplitters = zipWith
      (\splitter (spRN, stRN, dRN) -> splitter 0.95 spRN stRN dRN)
      (repeat randomSplitPolygon)
      combinedRNS

    rawSplits :: [[Polygon]]
    rawSplits = 
      -- TODO this algorithm is too ugly...
      scanl
        (flip ($))
        [seed]
        (take' depth (map (listUnion .) (map map randomizedSplitters)))
