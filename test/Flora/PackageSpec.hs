module Flora.PackageSpec where

import Data.Vector as Vector
import Optics.Core
import SpecHelpers (migrate)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted

import Flora.Model.Package
import Flora.PackageFixtures

spec :: Spec
spec = describeDB migrate "packages" $ do
  itDB "Insert base and its dependencies, and fetch it" $ do
    getPackageById (base ^. #packageId) `shouldReturn` Just base
  itDB "Fetch the dependents of array" $ do
    result <- getPackageDependants (array ^. #name) (array ^. #namespace)
    result `shouldBe` Vector.fromList [stm]
