import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit
import Data.Composition ((.:.))

import Statistics
import PolygonArt

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "for n >= 0, 0 < p <= 1 is the geometric CDF always <= 1.0" $
      -- TODO: replace condition with the correct input interval
      \p n -> 0 < p && p <= 1.0 ==> cumulativeGeometricDistribution p n <= 1.0
  ]

unitTests = testGroup "Unit tests" $
  let
    rectangle = polygon [Vertex 0 0, Vertex 2 0, Vertex 2 2, Vertex 0 2]
    triangle = polygon [Vertex 0 0, Vertex 2 0, Vertex 2 2]
    vertexEqual (Vertex a b) (Vertex x y) =
        fractionalEqual a x delta && fractionalEqual b y delta
      where delta = 0.0001
  in
    [
      testCase "Testing centroid calculation for a rectangle" $
          assertBool
            ""
            (vertexEqual
                (Vertex 1.0 1.0)
                (polygonCenter rectangle)
              ) 
    , testCase "Testing centroid calculation for a triangle" $
          assertBool
            ""
            (vertexEqual
                (Vertex 1.333333 0.666667)
                (polygonCenter triangle)
              ) 
    , testCase "Testing area calculation of a rectangle" $
          assertEqualFractional "" 4.0 (polygonArea rectangle) 0.00001
    , testCase "Testing area calculation of a triangle" $
          assertEqualFractional "" 2.0 (polygonArea triangle) 0.00001
    ]

fractionalEqual :: (Fractional a, Ord a) => a -> a -> a -> Bool
-- ^Checks whether two fractional numbers are equal within a 
-- error margin 'delta'
fractionalEqual expected actual delta = abs (expected - actual) <= delta

assertEqualFractional :: (HasCallStack, Ord a, Fractional a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> a      -- ^ delta comparison margin
                              -> Assertion
-- ^complements HUnit assertEqual. Can be used just the same, however
-- it is meant for testing the equality of fractionals within an error
-- margin 'delta'
assertEqualFractional preface = 
  assertBool preface .:. fractionalEqual
