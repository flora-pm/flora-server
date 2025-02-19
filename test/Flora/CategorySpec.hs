module Flora.CategorySpec where

import Test.Tasty

import Flora.Normalise (normaliseCategory)
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "category tuning"
    [ testThis "Test that the category unification algorithm works" testUnificationAlgorithm
    ]

testUnificationAlgorithm :: TestEff ()
testUnificationAlgorithm = do
  assertEqual
    (Just "Algorithms")
    (normaliseCategory "Algorithm")

  assertEqual
    (Just "Cryptography")
    (normaliseCategory "Crypto")

  assertEqual
    (Just "CLI & TUI Development")
    (normaliseCategory "CLI")

  assertEqual
    (Just "Mathematics")
    (normaliseCategory "Numeric")
