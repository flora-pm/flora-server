module Flora.AdvisorySpec where

import Data.Vector qualified as Vector
import RequireCallStack

import Advisories.Model.Advisory.Query qualified as Query
import Advisories.Model.Affected.Query qualified as Query
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Advisory tests"
    [ testThis "Fetch advisories by package id" testFetchingAllBiscuitHaskellAdvisories
    , testThis "Fetch advisory previews by package id" testFetchingAdvisoryPreviewByPackageId
    ]

testFetchingAllBiscuitHaskellAdvisories :: RequireCallStack => TestEff ()
testFetchingAllBiscuitHaskellAdvisories = do
  package <-
    assertJust_
      =<< Query.getPackageByNamespaceAndName
        (Namespace "hackage")
        (PackageName "biscuit-haskell")
  advisories <- Query.getAdvisoriesByPackageId package.packageId
  assertEqual_ 2 (Vector.length advisories)

testFetchingAllBaseAdvisories :: RequireCallStack => TestEff ()
testFetchingAllBaseAdvisories = do
  package <-
    assertJust_
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoriesByPackageId package.packageId
  assertEqual_ 1 (Vector.length advisories)

testFetchingAdvisoryPreviewByPackageId :: RequireCallStack => TestEff ()
testFetchingAdvisoryPreviewByPackageId = do
  package <-
    assertJust_
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoryPreviewsByPackageId package.packageId
  assertEqual_ 1 (Vector.length advisories)
