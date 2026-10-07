module Flora.AdvisorySpec where

import Data.Vector qualified as Vector
import Effectful.Reader.Static qualified as Reader
import RequireCallStack

import Advisories.Model.Advisory.Query qualified as Query
import Advisories.Model.Affected.Query qualified as Query
import Flora.Database
import Flora.Environment.Env
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
  FloraEnv{pool} <- Reader.ask
  package <-
    assertJust_
      =<< withReadOnlyPool
        pool
        ( Query.getPackageByNamespaceAndName
            (Namespace "local-hackage")
            (PackageName "biscuit-haskell")
        )
  advisories <- withReadOnlyPool pool $ Query.getAdvisoriesByPackageId package.packageId
  assertEqual_ 2 (Vector.length advisories)

testFetchingAllBaseAdvisories :: RequireCallStack => TestEff ()
testFetchingAllBaseAdvisories = do
  FloraEnv{pool} <- Reader.ask
  package <-
    assertJust_
      =<< withReadOnlyPool
        pool
        ( Query.getPackageByNamespaceAndName
            (Namespace "local-hackage")
            (PackageName "base")
        )
  advisories <- withReadOnlyPool pool $ Query.getAdvisoriesByPackageId package.packageId
  assertEqual_ 1 (Vector.length advisories)

testFetchingAdvisoryPreviewByPackageId :: RequireCallStack => TestEff ()
testFetchingAdvisoryPreviewByPackageId = do
  FloraEnv{pool} <- Reader.ask
  package <-
    assertJust_
      =<< withReadOnlyPool
        pool
        ( Query.getPackageByNamespaceAndName
            (Namespace "local-hackage")
            (PackageName "base")
        )
  advisories <- withReadOnlyPool pool $ Query.getAdvisoryPreviewsByPackageId package.packageId
  assertEqual_ 2 (Vector.length advisories)
