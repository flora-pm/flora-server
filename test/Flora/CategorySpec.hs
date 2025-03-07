module Flora.CategorySpec where

import Test.Tasty
import Data.Set qualified as Set

import Flora.Normalise
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Category Normalisation"
    [ testThis "Normalisation of Mathematics categories" testNormalisationOfMathematicsCategories
    ]

testNormalisationOfMathematicsCategories :: TestEff ()
testNormalisationOfMathematicsCategories = do
  let mathematicsExceptions =
        Set.fromList
          [ "Numeric"
          , "Numerical"
          , "Numerics"
          , "Arithmetic"
          , "Number Theory"
          , "Math"
          , "Mathematics"
          , "mathematics"
          , "Maths"
          , "Algebra"
          , "Graph"
          , "Graphs"
          , "Geometry"
          ]
  assertEqual
    (Set.singleton (Just "Mathematics"))
    (Set.map normaliseCategory mathematicsExceptions)
