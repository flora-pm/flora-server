module Flora.CabalSpec where

import Data.Maybe
import Data.Vector qualified as Vector
import Test.Tasty

import Flora.Model.Package
import Flora.Model.Package.Component
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "cabal tests"
    [ testThese
        "Components import"
        [ testThis "Import package with 1 public library and 1 executable" testImportSimplePackage
        , testThis "Import package with multiple public libraries" testImportMultiplePublicLibraries
        ]
    ]

testImportSimplePackage :: TestEff ()
testImportSimplePackage = do
  packageA <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "a")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual
    (fmap (. canonicalForm) componentsA)
    ( Vector.fromList
        [ CanonicalComponent{componentName = "a", componentType = Library}
        , CanonicalComponent{componentName = "e", componentType = Executable}
        ]
    )

testImportMultiplePublicLibraries :: TestEff ()
testImportMultiplePublicLibraries = do
  packageA <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "b")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual
    (fmap (. canonicalForm) componentsA)
    ( Vector.fromList
        [ CanonicalComponent{componentName = "b", componentType = Library}
        , CanonicalComponent{componentName = "sublib", componentType = Library}
        , CanonicalComponent{componentName = "anothersublib", componentType = Library}
        ]
    )
