{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.CategorySpec where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Debug.Pretty.Simple
import Optics.Core
import Test.Tasty

import Flora.Import.Categories.Tuning as Tuning
import Flora.Import.Package
import Flora.Import.Types
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.User
import Flora.TestUtils
import Flora.UserFixtures

spec :: TestM TestTree
spec = testThese "category tuning"
  [ testThis "Test that the category unification algorithm works" testUnificationAlgorithm
  ]

testUnificationAlgorithm :: TestM ()
testUnificationAlgorithm = do
  liftIO (Tuning.normalise [UserPackageCategory "Algorithm"])
    >>= assertEqual (Results [NormalisedPackageCategory "Algorithms"] [])

  liftIO (Tuning.normalise [UserPackageCategory "Crypto"])
    >>= assertEqual (Results [NormalisedPackageCategory "Cryptography"] [])

  liftIO (Tuning.normalise [UserPackageCategory "CLI", UserPackageCategory "TUI"])
    >>= assertEqual (Results [NormalisedPackageCategory "CLI & TUI Development"] [])

  liftIO (Tuning.normalise [UserPackageCategory "Numeric", UserPackageCategory "Parser Builder"])
    >>= assertEqual (Results [NormalisedPackageCategory "Mathematics",NormalisedPackageCategory "Parsers"] [])

