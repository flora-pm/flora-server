module Flora.AdvisorySpec where

import Advisories.Model.Advisory.Query qualified as Query
import Advisories.Model.Affected.Query qualified as Query
import Data.Vector qualified as Vector
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
  assertEqual (Vector.length advisories) 2

testFetchingAllBaseAdvisories :: TestEff ()
testFetchingAllBaseAdvisories = do
  package <-
    assertJust
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoriesByPackageId package.packageId
  assertEqual (Vector.length advisories) 1

testFetchingAdvisoryPreviewByPackageId :: TestEff ()
testFetchingAdvisoryPreviewByPackageId = do
  package <-
    assertJust
      =<< Query.getPackageByNamespaceAndName
        (Namespace "haskell")
        (PackageName "base")
  advisories <- Query.getAdvisoryPreviewsByPackageId package.packageId
  assertEqual (Vector.length advisories) 1
