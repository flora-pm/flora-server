module Flora.ImportSpec where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (void)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as Vector
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Shared qualified as State
import Log.Backend.StandardOutput (withStdOutLogger)
import RequireCallStack
import Streamly.Data.Stream qualified as Stream

import Flora.Database
import Flora.Domain.Import.Package (chooseNamespace)
import Flora.Domain.Import.Package.Bulk.Archive
import Flora.Domain.Import.Package.Bulk.Stream (importFromStream)
import Flora.Domain.Import.Types (ImportError (..), ImportFileType (..))
import Flora.Environment.Env
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
    , testThis "One bad cabal file among many good ones does not fail the import" testImportStreamSkipsBadFilesUnderThreshold
    , testThis "High failure rate trips the import circuit breaker" testImportStreamTripsCircuitBreakerOverThreshold
    , testThis "Repository with zero successful imports does not crash on NULL latest-release-time" testImportStreamAllFailuresUnderFloorDoesNotCrash
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
    withReadWritePool pool $ Update.upsertPackageIndex defaultRepo defaultRepoURL defaultDescription Nothing
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

resilienceGoodName :: Int -> String
resilienceGoodName n = "resilience-good" <> show n

goodCabalFile :: Int -> StrictByteString
goodCabalFile n =
  BS8.pack $
    unlines
      [ "cabal-version: 3.0"
      , "name: " <> resilienceGoodName n
      , "version: 1.0.0"
      , "build-type: Simple"
      , ""
      , "library"
      , "  exposed-modules: ResilienceGood"
      , "  build-depends: base"
      , "  default-language: Haskell2010"
      ]

badCabalFile :: StrictByteString
badCabalFile = "this is not a valid cabal file !!!"

runResilienceImport :: RequireCallStack => Text -> Int -> Int -> TestEff ()
runResilienceImport repo goodCount badCount = do
  FloraEnv{pool} <- Reader.ask
  withReadWritePool pool $ Update.upsertPackageIndex repo defaultRepoURL defaultDescription Nothing
  packageIndex <- assertJust_ =<< withReadOnlyPool pool (Query.getPackageIndexByName repo)
  let indexPackages =
        Vector.singleton
          ( repo
          , Set.fromList [PackageName (Text.pack (resilienceGoodName n)) | n <- [1 .. goodCount]]
          )
      epoch = posixSecondsToUTCTime 0
      subjects =
        [ (CabalFile (resilienceGoodName n <> ".cabal"), epoch, Nothing, goodCabalFile n)
        | n <- [1 .. goodCount]
        ]
          <> replicate badCount (CabalFile "resilience-bad.cabal", epoch, Nothing, badCabalFile)
  State.evalState mempty $
    importFromStream packageIndex indexPackages (Stream.fromList subjects)

testImportStreamSkipsBadFilesUnderThreshold :: RequireCallStack => TestEff ()
testImportStreamSkipsBadFilesUnderThreshold = do
  FloraEnv{pool} <- Reader.ask
  let repo = "resilience-test"
  runResilienceImport repo 99 1
  void . assertJust_
    =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace repo) (PackageName "resilience-good1"))

testImportStreamTripsCircuitBreakerOverThreshold :: RequireCallStack => TestEff ()
testImportStreamTripsCircuitBreakerOverThreshold = do
  result <- Error.tryError @ImportError $ runResilienceImport "resilience-test-cb" 19 1
  case result of
    Left (_, TooManyImportFailures failures total) -> assertEqual_ (1 :: Int, 20 :: Int) (failures, total)
    Left (_, err) -> assertFailure $ "expected TooManyImportFailures, got " <> show err
    Right () -> assertFailure "import unexpectedly succeeded"

testImportStreamAllFailuresUnderFloorDoesNotCrash :: RequireCallStack => TestEff ()
testImportStreamAllFailuresUnderFloorDoesNotCrash = do
  FloraEnv{pool} <- Reader.ask
  let repo = "resilience-test-allfail"
  runResilienceImport repo 0 5
  latestReleaseTime <- withReadOnlyPool pool $ Query.getLatestReleaseTime (Just repo)
  assertEqual "no release was ever persisted for this repository" Nothing latestReleaseTime

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
