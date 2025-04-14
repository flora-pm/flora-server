module Flora.AdvisorySpec where

import Data.Vector qualified as Vector

import Advisories.Model.Advisory.Query qualified as Query
import Advisories.Model.Affected.Query qualified as Query
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Advisory tests"
    [ testThis "Fetch advisories by package id" testFetchingAllBiscuitHaskellAdvisories
    , testThis "Fetch advisory previews by package id" testFetchingAdvisoryPreviewByPackageId
    ]

testFetchingAllBiscuitHaskellAdvisories :: TestEff ()
testFetchingAllBiscuitHaskellAdvisories = do
  package <-
    assertJust
      =<< Query.getPackageByNamespaceAndName
        (Namespace "hackage")
        (PackageName "biscuit-haskell")
  advisories <- Query.getAdvisoriesByPackageId package.packageId
  assertEqual 2 (Vector.length advisories)

testFetchingAllBaseAdvisories :: TestEff ()
testFetchingAllBaseAdvisories = do
  package <-
    assertJust
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoriesByPackageId package.packageId
  assertEqual 1 (Vector.length advisories)

testFetchingAdvisoryPreviewByPackageId :: TestEff ()
testFetchingAdvisoryPreviewByPackageId = do
  package <-
    assertJust
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoryPreviewsByPackageId package.packageId
  assertEqual 1 (Vector.length advisories)
