module Flora.ImportSpec where

import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Log.Backend.StandardOutput (withStdOutLogger)
import Optics.Core
import RequireCallStack

import Flora.Import.Package (chooseNamespace)
import Flora.Import.Package.Bulk.Archive (importFromArchive)
import Flora.Import.Package.Bulk.Directory (buildPackageListFromDirectory)
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Import tests"
    [ testThis "Import index" testImportIndex
    , testThis "Namespace chooser" testNamespaceChooser
    , testThis "MLabs dependencies in Cardano are correctly inserted" testNthLevelDependencies
    , testThis "Get a list of package names from a directory" testGetListOfPackageNamesFromDirectory
    ]

testIndex :: FilePath
testIndex = "./test/fixtures/tarballs/test-index.tar.gz"

defaultRepo :: Text
defaultRepo = "test-namespace"

defaultRepoURL :: Text
defaultRepoURL = "localhost"

defaultDescription :: Text
defaultDescription = "test-description"

testImportIndex :: RequireCallStack => TestEff ()
testImportIndex = withStdOutLogger $
  \_ -> do
    mIndex <- Query.getPackageIndexByName defaultRepo
    case mIndex of
      Nothing -> Update.createPackageIndex defaultRepo defaultRepoURL defaultDescription Nothing
      Just _ -> pure ()
    importFromArchive
      defaultRepo
      Vector.empty
      testIndex
    -- check the packages have been imported
    tars <- traverse (Query.getPackageByNamespaceAndName (Namespace defaultRepo) . PackageName) ["tar-a", "tar-b"]
    releases <- fmap mconcat . traverse (\x -> Query.getReleases (x ^. #packageId)) $ catMaybes tars
    assertEqual 2 (length tars)
    assertEqual 2 (length releases)
    traverse_ (\x -> assertEqual (x ^. #repository) (Just defaultRepo)) releases

testNamespaceChooser :: RequireCallStack => TestEff ()
testNamespaceChooser = do
  assertEqual
    (chooseNamespace (PackageName "tar-a") (Vector.singleton (defaultRepo, Set.fromList [PackageName "tar-a", PackageName "tar-b"])))
    (Just (Namespace defaultRepo))

testNthLevelDependencies :: RequireCallStack => TestEff ()
testNthLevelDependencies = do
  plutarch <- assertJust =<< Query.getPackageByNamespaceAndName (Namespace "mlabs") (PackageName "plutarch")
  latestRelease <- assertJust =<< Query.getLatestPackageRelease plutarch.packageId
  dependencies <- Query.getRequirements plutarch.name latestRelease.releaseId
  assertEqual
    ( Vector.fromList
        [ DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "base", version = ">=4.9 && <5"}
        , DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "bytestring", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "containers", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "mtl", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "random", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "haskell", packageName = PackageName "text", version = ">=0"}
        ]
    )
    dependencies

testGetListOfPackageNamesFromDirectory :: RequireCallStack => TestEff ()
testGetListOfPackageNamesFromDirectory = do
  result <- buildPackageListFromDirectory "test/fixtures/Cabal/mlabs"
  assertEqual
    (Set.fromList [PackageName "plutarch", PackageName "plutarch-ledger-api", PackageName "plutarch-orphanage"])
    result
