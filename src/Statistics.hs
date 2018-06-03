{-# LANGUAGE ScopedTypeVariables #-}

module Statistics where

import Numeric.Natural as Natural

cumulativeGeometricDistribution :: Float -> Natural -> Float
-- ^cumulative version (cdf) of the geomatric distribution.
-- https://en.wikipedia.org/wiki/Geometric_distribution
cumulativeGeometricDistribution p n = 1 - (1 - p)^(n+1)

weightedSelector :: forall a.
      (Natural -> Float) -- ^cumulative distribution function. Used to generate probabilities for the selection
    -> [a]               -- ^selection 
    -> (Float -> Maybe a)
-- ^generate a function, which returns an item from a selection by it's
-- probability.
weightedSelector cdf selection = select weightedSelection
  where
    -- generate an (infinite) list of weights by position within the
    -- selection
    weights = map cdf [0..]

    -- apply the weights to the selection
    weightedSelection = zip weights selection
    
    -- generated function as mentioned above.
    -- It searches the weighted selection until an item is found with
    -- a weight greater than the probability input
    --
    -- If none is found, the last item is returned, or nothing, if the
    -- selection is empty
    --
    -- Better, or even constant complexity could be achieved by a
    -- binary search or a HashMap or maybe even by an analytical
    -- approach. However, for now, this does the job
    select :: [(Float, a)] -> Float -> Maybe a
    select ((w, item):(x:xs)) p
      | p <= w     = Just item
      | otherwise = select (x:xs) p
    select (x:_) _ = Just (snd x)
    select [] _ = Nothing
