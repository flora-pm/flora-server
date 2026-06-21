module Flora.ImportSpec where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Effectful.Reader.Static qualified as Reader
import Log.Backend.StandardOutput (withStdOutLogger)
import RequireCallStack

import Flora.Database
import Flora.Environment.Env
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
    , testThis "Cardano dependencies are preferred in Cardano, then in Hackage" testCardanoDependencyResolution
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
    FloraEnv{pool} <- Reader.ask
    mIndex <- withReadOnlyPool pool $ Query.getPackageIndexByName defaultRepo
    case mIndex of
      Nothing -> withReadWritePool pool $ Update.createPackageIndex defaultRepo defaultRepoURL defaultDescription Nothing
      Just _ -> pure ()
    importFromArchive
      "test-namespace"
      Vector.empty
      "test/fixtures"

    -- check the packages have been imported
    tars <- traverse (\p -> withReadOnlyPool pool $ Query.getPackageByNamespaceAndName (Namespace defaultRepo) (PackageName p)) ["tar-a", "tar-b"]
    releases <- mconcat <$> traverse (\x -> withReadOnlyPool pool (Query.getReleases x.packageId)) (catMaybes tars)
    assertEqual_ 2 (length tars)
    assertEqual_ 2 (length releases)
    traverse_ (\x -> assertEqual_ x.repository (Just defaultRepo)) releases

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
  FloraEnv{pool} <- Reader.ask
  plutarch <- assertJust_ =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace "mlabs") (PackageName "plutarch"))
  latestRelease <- assertJust_ =<< withReadOnlyPool pool (Query.getLatestPackageRelease plutarch.packageId)
  dependencies <- Set.fromList . Vector.toList <$> withReadOnlyPool pool (Query.getRequirements plutarch.name latestRelease.releaseId)
  assertEqual_
    ( Set.fromList
        [ DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "aeson", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "base", version = ">=4.9 && <5"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "bytestring", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "constraints", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "containers", version = ">=0"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "cryptonite", version = ">=0"}
        ]
    )
    dependencies

testCardanoDependencyResolution :: RequireCallStack => TestEff ()
testCardanoDependencyResolution = do
  FloraEnv{pool} <- Reader.ask
  strictCheckedVars <-
    assertJust_
      =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace "cardano") (PackageName "strict-checked-vars"))
  latestRelease <- assertJust_ =<< withReadOnlyPool pool (Query.getLatestPackageRelease strictCheckedVars.packageId)
  dependencies <- Set.fromList . Vector.toList <$> withReadOnlyPool pool (Query.getRequirements strictCheckedVars.name latestRelease.releaseId)
  assertEqual_
    ( Set.fromList
        [ DependencyVersionRequirement{namespace = Namespace "cardano", packageName = PackageName "io-classes", version = ">=1.2 && <1.6"}
        , DependencyVersionRequirement{namespace = Namespace "cardano", packageName = PackageName "strict-mvar", version = ">=1.2 && <1.6"}
        , DependencyVersionRequirement{namespace = Namespace "cardano", packageName = PackageName "strict-stm", version = ">=1.2 && <1.6"}
        , DependencyVersionRequirement{namespace = Namespace "local-hackage", packageName = PackageName "base", version = ">=4.9 && <5"}
        ]
    )
    dependencies
