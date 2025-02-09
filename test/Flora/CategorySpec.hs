module Flora.CategorySpec where

import Control.Monad.IO.Class
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
  liftIO (normaliseCategory "Algorithm")
    >>= assertEqual "Algorithms"

  liftIO (normaliseCategory "Crypto")
    >>= assertEqual "Cryptography"

  liftIO (normaliseCategory "CLI")
    >>= assertEqual "CLI & TUI Development"

  liftIO (normaliseCategory "Numeric")
    >>= assertEqual "Mathematics"
