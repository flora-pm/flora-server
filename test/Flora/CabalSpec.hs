module Flora.CabalSpec where

import Data.Maybe
import Data.Set qualified as Set
import RequireCallStack
import Data.Vector qualified as Vector
import Test.Tasty

import Flora.Model.Component.Types
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "cabal tests"
    [ testThese
        "Components import"
        [ testThis "Import package with 1 public library and 1 executable" testImportSimplePackage
        , testThis "Import package with multiple public libraries" testImportMultiplePublicLibraries
        ]
    ]

testImportSimplePackage :: RequireCallStack => TestEff ()
testImportSimplePackage = do
  packageA <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "a")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ CanonicalComponent{componentName = "a", componentType = Library}
        , CanonicalComponent{componentName = "e", componentType = Executable}
        ]
    )

testImportMultiplePublicLibraries :: RequireCallStack => TestEff ()
testImportMultiplePublicLibraries = do
  packageA <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "b")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ CanonicalComponent{componentName = "b", componentType = Library}
        , CanonicalComponent{componentName = "sublib", componentType = Library}
        , CanonicalComponent{componentName = "anothersublib", componentType = Library}
        ]
    )
