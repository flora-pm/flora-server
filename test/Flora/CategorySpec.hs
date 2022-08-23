{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.CategorySpec where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Pretty.Simple
import Optics.Core
import Test.Tasty

import Flora.Import.Categories.Tuning as Tuning
import Flora.Import.Package
import Flora.Import.Types
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.User
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "category tuning"
    [ testThis "Test that the category unification algorithm works" testUnificationAlgorithm
    ]

testUnificationAlgorithm :: TestEff ()
testUnificationAlgorithm = do
  liftIO (Tuning.normalise [UserPackageCategory "Algorithm"])
    >>= assertEqual (Results [NormalisedPackageCategory "Algorithms"] [])

  liftIO (Tuning.normalise [UserPackageCategory "Crypto"])
    >>= assertEqual (Results [NormalisedPackageCategory "Cryptography"] [])

  liftIO (Tuning.normalise [UserPackageCategory "CLI", UserPackageCategory "TUI"])
    >>= assertEqual (Results [NormalisedPackageCategory "CLI & TUI Development"] [])

  liftIO (Tuning.normalise [UserPackageCategory "Numeric", UserPackageCategory "Parser Builder"])
    >>= assertEqual (Results [NormalisedPackageCategory "Mathematics", NormalisedPackageCategory "Parsers"] [])
