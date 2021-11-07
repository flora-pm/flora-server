module Flora.PackageSpec where

import Optics.Core
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)

import Flora.Model.Package
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures
import SpecHelpers (migrate)

spec :: Spec
spec = describeDB migrate "packages" $ do
  itDB "Insert base and its dependencies, and fetch it" $ do
    insertUser ben
    insertUser syl20
    publishPackage [] ghcPrimRelease ghcPrim syl20
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum ben
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base ben
    getPackageById (base ^. #packageId) `shouldReturn` Just base
