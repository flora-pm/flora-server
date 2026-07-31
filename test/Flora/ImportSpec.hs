module Flora.ImportSpec where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, void)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as Vector
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Log.Backend.StandardOutput (withStdOutLogger)
import RequireCallStack
import Streamly.Data.Stream qualified as Stream

import Flora.Database
import Flora.Domain.Import.Package (chooseNamespace)
import Flora.Domain.Import.Package.Bulk.Archive
import Flora.Domain.Import.Package.Bulk.Stream (importFromStream, importWorkerLimit)
import Flora.Domain.Import.Types (ImportError (..), ImportFileType (..))
import Flora.Environment.Env
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types
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
    , testThis "Scanning an index collects every package name, whatever the cut-off" testScanIndexKeepsAllPackageNames
    , testThis "An incremental import still resolves dependencies on packages it skipped" testIncrementalImportResolvesSkippedDependencies
    , testThis "Only the newest revision of a cabal file is imported" testImportKeepsNewestCabalRevision
    , testThis "Mutually-dependent packages import concurrently without deadlocking" testConcurrentImportOfMutuallyDependentPackages
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
  entries <- readIndexFixture "Cabal/mlabs"
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

cliqueRepo :: Text
cliqueRepo = "clique-index"

cliqueMaintainer :: Text
cliqueMaintainer = "clique-maintainer"

cliqueSize :: Int
cliqueSize = 24

cliqueName :: Int -> String
cliqueName n = "clique-pkg" <> show n

cliqueCabalFile :: Int -> StrictByteString
cliqueCabalFile n =
  BS8.pack $
    unlines
      [ "cabal-version: 3.0"
      , "name: " <> cliqueName n
      , "version: 1.0.0"
      , "build-type: Simple"
      , ""
      , "library"
      , "  exposed-modules: Clique"
      , "  build-depends: " <> intercalate ", " [cliqueName other | other <- [1 .. cliqueSize], other /= n]
      , "  default-language: Haskell2010"
      , ""
      , "test-suite spec"
      , "  type: exitcode-stdio-1.0"
      , "  main-is: Main.hs"
      , "  build-depends: " <> cliqueName n
      , "  default-language: Haskell2010"
      ]

testConcurrentImportOfMutuallyDependentPackages :: RequireCallStack => TestEff ()
testConcurrentImportOfMutuallyDependentPackages = do
  FloraEnv{pool, dbConfig} <- Reader.ask
  unless (importWorkerLimit dbConfig >= 2) $
    assertFailure "this test needs a connection pool big enough for concurrent workers"
  withReadWritePool pool $ Update.upsertPackageIndex cliqueRepo defaultRepoURL defaultDescription Nothing
  packageIndex <- assertJust_ =<< withReadOnlyPool pool (Query.getPackageIndexByName cliqueRepo)
  let cliquePackageNames = [PackageName (Text.pack (cliqueName n)) | n <- [1 .. cliqueSize]]
      indexPackages = Vector.singleton (cliqueRepo, Set.fromList cliquePackageNames)
      epoch = posixSecondsToUTCTime 0
      subjects =
        [ (CabalFile (cliqueName n <> ".cabal"), epoch, Just cliqueMaintainer, cliqueCabalFile n)
        | n <- [1 .. cliqueSize]
        ]
  importFromStream packageIndex indexPackages (Stream.fromList subjects)

  imported <-
    catMaybes
      <$> traverse
        (withReadOnlyPool pool . Query.getPackageByNamespaceAndName (Namespace cliqueRepo))
        cliquePackageNames
  assertEqual "every package of the clique was imported" cliqueSize (length imported)

  uploader <-
    assertJust_
      =<< withReadOnlyPool pool (Query.getPackageUploaderByUsernameAndIndex cliqueMaintainer packageIndex.packageIndexId)
  releases <- traverse requireLatestRelease imported
  traverse_ (\r -> assertEqual_ (Just uploader.packageUploaderId) r.uploaderId) releases

  traverse_ (\p -> assertEqual_ FullyImportedPackage p.status) imported

incrementalRepo :: Text
incrementalRepo = "incremental-index"

incrementalCutOff :: UTCTime
incrementalCutOff = UTCTime (fromGregorian 2020 1 1) 0

staleCabalPath :: Text
staleCabalPath = "stale-pkg/1.0.0/stale-pkg.cabal"

recentCabalPath :: Text
recentCabalPath = "recent-pkg/1.0.0/recent-pkg.cabal"

readIndexFixture :: Text -> TestEff (Tar.Entries Tar.FormatError)
readIndexFixture repo =
  Tar.read . GZip.decompress
    <$> liftIO (BL.readFile ("test/fixtures/" <> Text.unpack repo <> "/01-index.tar.gz"))

importFixtureIndex :: RequireCallStack => Text -> Maybe UTCTime -> TestEff ()
importFixtureIndex repo timestamp = do
  FloraEnv{pool} <- Reader.ask
  withReadWritePool pool $
    Update.upsertPackageIndex repo defaultRepoURL defaultDescription timestamp
  importFromArchive repo Vector.empty "test/fixtures"

requirePackage :: RequireCallStack => Text -> Text -> TestEff Package
requirePackage namespace name = do
  FloraEnv{pool} <- Reader.ask
  assertJust_
    =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace namespace) (PackageName name))

requireLatestRelease :: RequireCallStack => Package -> TestEff Release
requireLatestRelease package = do
  FloraEnv{pool} <- Reader.ask
  assertJust_ =<< withReadOnlyPool pool (Query.getLatestPackageRelease package.packageId)

testScanIndexKeepsAllPackageNames :: RequireCallStack => TestEff ()
testScanIndexKeepsAllPackageNames = do
  entries <- readIndexFixture incrementalRepo
  (namesFromEpoch, countsFromEpoch) <- assertRight $ scanIndex (posixSecondsToUTCTime 0) entries
  (namesFromCutOff, countsFromCutOff) <- assertRight $ scanIndex incrementalCutOff entries

  assertEqual_ (Set.fromList [PackageName "recent-pkg", PackageName "stale-pkg"]) namesFromEpoch
  assertEqual "the cut-off must not narrow the package names" namesFromEpoch namesFromCutOff

  assertEqual_ [recentCabalPath, staleCabalPath] (Map.keys countsFromEpoch)
  assertEqual_ [recentCabalPath] (Map.keys countsFromCutOff)

testIncrementalImportResolvesSkippedDependencies :: RequireCallStack => TestEff ()
testIncrementalImportResolvesSkippedDependencies = withStdOutLogger $ \_ -> do
  FloraEnv{pool} <- Reader.ask
  importFixtureIndex incrementalRepo (Just incrementalCutOff)

  recentPkg <- requirePackage incrementalRepo "recent-pkg"
  release <- requireLatestRelease recentPkg
  dependencies <-
    Set.fromList . Vector.toList
      <$> withReadOnlyPool pool (Query.getRequirements recentPkg.name release.releaseId)

  assertEqual_
    ( Set.singleton
        DependencyVersionRequirement
          { namespace = Namespace incrementalRepo
          , packageName = PackageName "stale-pkg"
          , version = ">=0"
          }
    )
    dependencies

  stalePkg <- requirePackage incrementalRepo "stale-pkg"
  staleReleases <- withReadOnlyPool pool $ Query.getReleases stalePkg.packageId
  assertEqual "stale-pkg was out of scope and must not have been imported" 0 (length staleReleases)

revisedRepo :: Text
revisedRepo = "revised-index"

testImportKeepsNewestCabalRevision :: RequireCallStack => TestEff ()
testImportKeepsNewestCabalRevision = withStdOutLogger $ \_ -> do
  FloraEnv{pool} <- Reader.ask
  importFixtureIndex revisedRepo Nothing

  revisedPkg <- requirePackage revisedRepo "revised-pkg"
  releases <- withReadOnlyPool pool $ Query.getReleases revisedPkg.packageId
  assertEqual "both entries are the same version, so there is one release" 1 (length releases)
  release <- requireLatestRelease revisedPkg
  assertEqual "the newest revision is the one that must be stored" "revision 1" release.synopsis

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
