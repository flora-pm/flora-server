module Flora.PackageSpec where

import Data.Vector as Vector
import Debug.Trace
import Optics.Core
import SpecHelpers (migrate)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted

import Flora.Model.Package
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

spec :: Spec
spec = describeDB migrate "packages" $ do
  itDB "Insert base and its dependencies, and fetch it" $ do
    insertUser ben
    insertUser syl20
    publishPackage [] ghcPrimRelease ghcPrim syl20
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum ben
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base ben
    getPackageById (base ^. #packageId) `shouldReturn` Just base
  itDB "Fetch the dependents of ghc-prim" $ do
    result <- getPackageDependants (ghcPrim ^. #name) (ghcPrim ^. #namespace)
    traceShowM result
    result `shouldBe` Vector.fromList [base]
