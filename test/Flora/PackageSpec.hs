module Flora.PackageSpec where

import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)
import Optics.Core

import SpecHelpers (migrate)
import Flora.Model.User
import Flora.Model.Package
import Flora.Publish
import Flora.PackageFixtures
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
