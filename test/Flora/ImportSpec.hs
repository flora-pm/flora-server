module Flora.ImportSpec where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Log.Backend.StandardOutput (withStdOutLogger)
import Optics.Core
import RequireCallStack

import Flora.Import.Package (chooseNamespace)
import Flora.Import.Package.Bulk.Archive
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
    [ testThis "Namespace chooser" testNamespaceChooser
    , testThis "Import index" testImportIndex
    , testThis "Package list from archive" testPackageListFromArchive
    , testThis "MLabs dependencies in Cardano are correctly inserted" testNthLevelDependencies
    ]

testIndex :: FilePath
testIndex = "test/fixtures/test-namespace/test-index.tar.gz"

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
      "test-namespace"
      Vector.empty
      "test/fixtures"

    -- check the packages have been imported
    tars <- traverse (Query.getPackageByNamespaceAndName (Namespace defaultRepo) . PackageName) ["tar-a", "tar-b"]
    releases <- fmap mconcat . traverse (\x -> Query.getReleases (x ^. #packageId)) $ catMaybes tars
    assertEqual_ 2 (length tars)
    assertEqual_ 2 (length releases)
    traverse_ (\x -> assertEqual_ (x ^. #repository) (Just defaultRepo)) releases

testNamespaceChooser :: RequireCallStack => TestEff ()
testNamespaceChooser = do
  assertEqual_
    (chooseNamespace (PackageName "tar-a") (Vector.singleton (defaultRepo, Set.fromList [PackageName "tar-a", PackageName "tar-b"])))
    (Just (Namespace defaultRepo))

testPackageListFromArchive :: RequireCallStack => TestEff ()
testPackageListFromArchive = do
  entries <- Tar.read . GZip.decompress <$> liftIO (BL.readFile "test/fixtures/Cabal/mlabs/01-index.tar.gz")
  packages <- assertRight $ buildPackageListFromArchive entries

  assertEqual_
    (Set.fromList [PackageName "plutarch", PackageName "plutarch-ledger-api", PackageName "plutarch-orphanage"])
    packages

testNthLevelDependencies :: RequireCallStack => TestEff ()
testNthLevelDependencies = do
  plutarch <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "mlabs") (PackageName "plutarch")
  latestRelease <- assertJust_ =<< Query.getLatestPackageRelease plutarch.packageId
  dependencies <- Set.fromList . Vector.toList <$> Query.getRequirements plutarch.name latestRelease.releaseId
  assertEqual_
    ( Set.fromList
        [DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "aeson", version = ">=0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "base", version = ">=4.9 && <5"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "bytestring", version = ">=0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "constraints", version = ">=0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "containers", version = ">=0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "cryptonite", version = ">=0"}]
    )
    dependencies
