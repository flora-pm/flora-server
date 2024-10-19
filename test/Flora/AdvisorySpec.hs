module Flora.AdvisorySpec where

import Advisories.Model.Advisory.Query qualified as Query
import Data.Vector qualified as Vector
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Advisory tests"
    [ testThis "Fetching specific advisories by package id" testFetchingAllBiscuitHaskellAdvisories
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
