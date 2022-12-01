{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-|
Module: Flora.Import.Package

This module contains all the code to import Cabal packages into Flora. The import process
for a single package is divided in three consecutive steps:

  1. The Cabal file is read from the file system and parsed into a 'GenericPackageDescription' from the Cabal package
  2. Relevant data from the Cabal package is extracted and turned into an intermediate representation, 'ImportOutput'
  3. This 'ImportOutput' is inserted (or more precisely upserted) into the database

We strive to keep step 2 deterministic and side-effect free, besides accessing the current time and logging.
We also want to keep the import procedure idempotent.

Packages can be imported in any order, even before their dependencies are known. When importing a package,
any dependency that isn't yet known will be imported as an "unknown package", as indicated by its status field.
If and when that package is fully imported later, we complete its data and change its status to "fully imported" without
altering its id.
-}
module Flora.Import.Package where

import Control.Exception
import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Pool (Pool)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Display
import Data.Text.IO qualified as T
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import Distribution.Fields.ParseResult
import Distribution.PackageDescription (CondBranch (..), CondTree (condTreeData), Condition (CNot), ConfVar, UnqualComponentName, allLibraries, unPackageName, unUnqualComponentName)
import Distribution.PackageDescription qualified as Cabal hiding (PackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Pretty
import Distribution.Types.Benchmark
import Distribution.Types.Dependency
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.PackageDescription ()
import Distribution.Types.TestSuite
import Distribution.Utils.ShortText qualified as Cabal
import Effectful
import Effectful.Internal.Monad (unsafeEff_)
import Effectful.PostgreSQL.Transact.Effect (DB, getPool, runDB)
import Effectful.Time (Time)
import GHC.Generics (Generic)
import Log qualified
import Optics.Core
import Streamly.Prelude qualified as S
import System.Directory qualified as System
import System.FilePath

import Control.DeepSeq (NFData, force)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, withinRange)
import Distribution.Version qualified as Version
import Effectful.Log (Log)
import Flora.Import.Categories.Tuning qualified as Tuning
import Flora.Import.Types
import Flora.Model.Category.Update qualified as Update
import Flora.Model.Package.Component as Component
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release (deterministicReleaseId)
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement
  ( Requirement (..)
  , RequirementMetadata (..)
  , deterministicRequirementId
  , flag
  )
import Flora.Model.User
import GHC.Stack (HasCallStack)

{-| This tuple represents the package that depends on any associated dependency/requirement.
 It is used in the recursive loading of Cabal files
-}
type DependentName = (Namespace, PackageName)

type ImportComponent = (PackageComponent, [ImportDependency])

data ImportDependency = ImportDependency
  { package :: Package
  -- ^ the package that is being depended on. Must be inserted in the DB before the requirement
  , requirement :: Requirement
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ImportOutput = ImportOutput
  { package :: Package
  , categories :: [Tuning.NormalisedPackageCategory]
  , release :: Release
  , components :: [ImportComponent]
  }
  deriving stock (Eq, Show, Generic)

coreLibraries :: Set PackageName
coreLibraries =
  Set.fromList
    [ PackageName "Cabal"
    , PackageName "Win32"
    , PackageName "array"
    , PackageName "base"
    , PackageName "binary"
    , PackageName "bytestring"
    , PackageName "containers"
    , PackageName "deepseq"
    , PackageName "directory"
    , PackageName "entropy"
    , PackageName "filepath"
    , PackageName "ghc-bignum"
    , PackageName "ghc-boot-th"
    , PackageName "ghc-prim"
    , PackageName "integer-gmp"
    , PackageName "integer-simple"
    , PackageName "mtl"
    , PackageName "parallel"
    , PackageName "parsec"
    , PackageName "primitive"
    , PackageName "process"
    , PackageName "random"
    , PackageName "rts"
    , PackageName "stm"
    , PackageName "template-haskell"
    , PackageName "text"
    , PackageName "transformers"
    , PackageName "unix"
    , PackageName "vector"
    ]

versionList :: Set Version
versionList =
  Set.fromList
    [ Version.mkVersion [9, 4, 1]
    , Version.mkVersion [9, 2, 4]
    , Version.mkVersion [9, 2, 3]
    , Version.mkVersion [9, 2, 2]
    , Version.mkVersion [9, 2, 1]
    , Version.mkVersion [9, 0, 2]
    , Version.mkVersion [9, 0, 1]
    , Version.mkVersion [8, 10, 7]
    , Version.mkVersion [8, 10, 6]
    , Version.mkVersion [8, 10, 5]
    , Version.mkVersion [8, 10, 4]
    , Version.mkVersion [8, 10, 3]
    , Version.mkVersion [8, 10, 2]
    , Version.mkVersion [8, 10, 1]
    , Version.mkVersion [8, 8, 4]
    , Version.mkVersion [8, 8, 3]
    , Version.mkVersion [8, 8, 2]
    , Version.mkVersion [8, 8, 1]
    , Version.mkVersion [8, 6, 5]
    , Version.mkVersion [8, 6, 4]
    , Version.mkVersion [8, 6, 3]
    , Version.mkVersion [8, 6, 2]
    , Version.mkVersion [8, 6, 1]
    , Version.mkVersion [8, 4, 4]
    , Version.mkVersion [8, 4, 3]
    , Version.mkVersion [8, 4, 2]
    , Version.mkVersion [8, 4, 1]
    , Version.mkVersion [8, 2, 2]
    , Version.mkVersion [8, 0, 2]
    , Version.mkVersion [7, 10, 3]
    ]

{-| Imports a Cabal file into the database by:
   * first, reading and parsing the file using 'loadFile'
   * then, extracting relevant information using 'extractPackageDataFromCabal'
   * finally, inserting that data into the database
-}
importFile
  :: (DB :> es, IOE :> es, Log :> es, Time :> es)
  => UserId
  -> FilePath
  -- ^ The absolute path to the Cabal file
  -> Eff es ()
importFile userId path = loadFile path >>= extractPackageDataFromCabal userId >>= persistImportOutput

importRelFile :: (DB :> es, IOE :> es, Log :> es, Time :> es) => UserId -> FilePath -> Eff es ()
importRelFile user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importFile user workdir

-- | Loads and parses a Cabal file
loadFile
  :: (DB :> es, IOE :> es, Log :> es, Time :> es)
  => FilePath
  -- ^ The absolute path to the Cabal file
  -> Eff es GenericPackageDescription
loadFile path = do
  exists <- liftIO $ System.doesFileExist path
  unless exists $
    unsafeEff_ $
      throwIO $
        CabalFileNotFound path
  content <- liftIO $ BS.readFile path
  parseString parseGenericPackageDescription path content

parseString
  :: (HasCallStack, Log :> es, Time :> es)
  => (BS.ByteString -> ParseResult a)
  -- ^ File contents to final value parser
  -> String
  -- ^ File name
  -> BS.ByteString
  -> Eff es a
parseString parser name bs = do
  let (_warnings, result) = runParseResult (parser bs)
  case result of
    Right x -> pure x
    Left err -> do
      Log.logAttention_ (display $ show err)
      throw $ CabalFileCouldNotBeParsed name

loadAndExtractCabalFile :: (DB :> es, IOE :> es, Log :> es, Time :> es) => UserId -> FilePath -> Eff es ImportOutput
loadAndExtractCabalFile userId filePath = loadFile filePath >>= extractPackageDataFromCabal userId

{-| Persists an 'ImportOutput' to the database. An 'ImportOutput' can be obtained
 by extracting relevant information from a Cabal file using 'extractPackageDataFromCabal'
-}
persistImportOutput :: (DB :> es, IOE :> es) => ImportOutput -> Eff es ()
persistImportOutput (ImportOutput package categories release components) = do
  pool <- getPool
  liftIO . T.putStrLn $ "📦  Persisting package: " <> packageName <> ", 🗓  Release v" <> display (release.version)
  persistPackage
  Update.upsertRelease release
  parallelRun pool (persistComponent pool) components
  liftIO $ putStr "\n"
  where
    parallelRun :: (MonadIO m, Foldable t) => Pool Connection -> (a -> Eff [DB, IOE] b) -> t a -> m ()
    parallelRun pool f = force $ liftIO . S.drain . S.fromParallel . S.mapM (runEff . runDB pool . f) . S.fromFoldable
    packageName = display (package.namespace) <> "/" <> display (package.name)
    persistPackage = do
      let packageId = package.packageId
      Update.upsertPackage package
      forM_ categories (\case Tuning.NormalisedPackageCategory cat -> Update.addToCategoryByName packageId cat)

    persistComponent pool (packageComponent, deps) = do
      liftIO . T.putStrLn $
        "🧩  Persisting component: " <> display (packageComponent.canonicalForm) <> " with " <> display (length deps) <> " dependencies."
      Update.upsertPackageComponent packageComponent
      parallelRun pool persistImportDependency deps

    persistImportDependency dep = do
      Update.upsertPackage (dep.package)
      Update.upsertRequirement (dep.requirement)

{-| Transforms a 'GenericPackageDescription' from Cabal into an 'ImportOutput'
 that can later be inserted into the database. This function produces stable, deterministic ids,
 so it should be possible to extract and insert a single package many times in a row.
-}
extractPackageDataFromCabal :: (DB :> es, IOE :> es) => UserId -> GenericPackageDescription -> Eff es ImportOutput
extractPackageDataFromCabal userId genericDesc = do
  let packageDesc = genericDesc.packageDescription
  let flags = Vector.fromList genericDesc.genPackageFlags
  let packageName = force $ packageDesc ^. #package % #pkgName % to unPackageName % to pack % to PackageName
  let packageVersion = force $ packageDesc.package.pkgVersion
  let namespace = force $ chooseNamespace packageName
  let packageId = deterministicPackageId namespace packageName
  let releaseId = deterministicReleaseId packageId packageVersion
  timestamp <- liftIO getCurrentTime
  let sourceRepos = getRepoURL packageName $ packageDesc.sourceRepos
  let rawCategoryField = packageDesc ^. #category % to Cabal.fromShortText % to T.pack
  let categoryList = fmap (Tuning.UserPackageCategory . T.stripStart . T.stripEnd) (T.splitOn "," rawCategoryField)
  categories <- liftIO $ Tuning.normalisedCategories <$> Tuning.normalise categoryList
  let package =
        Package
          { packageId
          , namespace
          , name = packageName
          , ownerId = userId
          , createdAt = timestamp
          , updatedAt = timestamp
          , status = FullyImportedPackage
          }

  let metadata =
        ReleaseMetadata
          { license = Cabal.license packageDesc
          , sourceRepos
          , homepage = Just $ display packageDesc.homepage
          , documentation = ""
          , bugTracker = Just $ display packageDesc.bugReports
          , maintainer = display packageDesc.maintainer
          , synopsis = display packageDesc.synopsis
          , description = display packageDesc.description
          , flags = flags
          , testedWith = getVersions . extractTestedWith . Vector.fromList $ packageDesc.testedWith
          }

  let release =
        Release
          { releaseId
          , packageId
          , version = packageVersion
          , archiveChecksum = mempty
          , metadata = metadata
          , uploadedAt = Nothing
          , createdAt = timestamp
          , updatedAt = timestamp
          , readme = Nothing
          , readmeStatus = NotImported
          , changelog = Nothing
          , changelogStatus = NotImported
          }

  let lib = extractLibrary package release Nothing [] <$> allLibraries packageDesc
  let condLib = maybe [] (extractCondTree extractLibrary package release Nothing) (genericDesc.condLibrary)
  let condSubLibs = extractCondTrees extractLibrary package release genericDesc.condSubLibraries

  let foreignLibs = extractForeignLib package release Nothing [] <$> packageDesc.foreignLibs
  let condForeignLibs = extractCondTrees extractForeignLib package release genericDesc.condForeignLibs

  let executables = extractExecutable package release Nothing [] <$> packageDesc.executables
  let condExecutables = extractCondTrees extractExecutable package release genericDesc.condExecutables

  let testSuites = extractTestSuite package release Nothing [] <$> packageDesc.testSuites
  let condTestSuites = extractCondTrees extractTestSuite package release genericDesc.condTestSuites

  let benchmarks = extractBenchmark package release Nothing [] <$> packageDesc.benchmarks
  let condBenchmarks = extractCondTrees extractBenchmark package release genericDesc.condBenchmarks

  let components =
        lib
          <> condLib
          <> condSubLibs
          <> executables
          <> condExecutables
          <> foreignLibs
          <> condForeignLibs
          <> testSuites
          <> condTestSuites
          <> benchmarks
          <> condBenchmarks
  pure ImportOutput{..}

extractLibrary :: Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> Library -> ImportComponent
extractLibrary package =
  genericComponentExtractor
    Component.Library
    (^. #libName % to getLibName)
    (^. #libBuildInfo % #targetBuildDepends)
    package
  where
    getLibName :: LibraryName -> Text
    getLibName LMainLibName = display (package.name)
    getLibName (LSubLibName lname) = T.pack $ unUnqualComponentName lname

extractForeignLib :: Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> ForeignLib -> ImportComponent
extractForeignLib package =
  genericComponentExtractor
    Component.ForeignLib
    (^. #foreignLibName % to unUnqualComponentName % to T.pack)
    (^. #foreignLibBuildInfo % #targetBuildDepends)
    package

extractExecutable :: Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> Executable -> ImportComponent
extractExecutable =
  genericComponentExtractor
    Component.Executable
    (^. #exeName % to unUnqualComponentName % to T.pack)
    (^. #buildInfo % #targetBuildDepends)

extractTestSuite :: Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> TestSuite -> ImportComponent
extractTestSuite =
  genericComponentExtractor
    Component.TestSuite
    (^. #testName % to unUnqualComponentName % to T.pack)
    (^. #testBuildInfo % #targetBuildDepends)

extractBenchmark :: Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> Benchmark -> ImportComponent
extractBenchmark =
  genericComponentExtractor
    Component.Benchmark
    (^. #benchmarkName % to unUnqualComponentName % to T.pack)
    (^. #benchmarkBuildInfo % #targetBuildDepends)

{-| Traverses the provided 'CondTree' and applies the given 'ComponentExtractor'
 to every node, returning a list of 'ImportComponent'
-}
extractCondTree
  :: (Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> component -> ImportComponent)
  -> Package
  -> Release
  -> Maybe UnqualComponentName
  -> CondTree ConfVar [Dependency] component
  -> [ImportComponent]
extractCondTree extractor package release defaultComponentName = go []
  where
    go cond tree =
      let treeComponent = extractor package release defaultComponentName cond $ tree.condTreeData
          treeSubComponents = (tree.condTreeComponents) >>= extractBranch
       in treeComponent : treeSubComponents
    extractBranch CondBranch{condBranchCondition, condBranchIfTrue, condBranchIfFalse} =
      let condIfTrueComponents = go [condBranchCondition] condBranchIfTrue
          condIfFalseComponents = maybe [] (go [CNot condBranchCondition]) condBranchIfFalse
       in condIfTrueComponents <> condIfFalseComponents

{-| Cabal often models conditional components as a list of 'CondTree' associated with an 'UnqualComponentName'.
 This function builds upon 'extractCondTree' to make it easier to extract fields such as 'condExecutables', 'condTestSuites' etc.
 from a 'GenericPackageDescription'
-}
extractCondTrees
  :: (Package -> Release -> Maybe UnqualComponentName -> [Condition ConfVar] -> component -> ImportComponent)
  -> Package
  -> Release
  -> [(UnqualComponentName, CondTree ConfVar [Dependency] component)]
  -> [ImportComponent]
extractCondTrees extractor package release trees =
  trees >>= \case (name, tree) -> extractCondTree extractor package release (Just name) tree

genericComponentExtractor
  :: forall component
   . ()
  => ComponentType
  -> (component -> Text)
  -- ^ Extract name from component
  -> (component -> [Dependency])
  -- ^ Extract dependencies
  -> Package
  -> Release
  -> Maybe UnqualComponentName
  -> [Condition ConfVar]
  -> component
  -> (PackageComponent, [ImportDependency])
genericComponentExtractor
  componentType
  getName
  getDeps
  package
  release
  defaultComponentName
  condition
  rawComponent =
    let releaseId = release.releaseId
        componentName = maybe (getName rawComponent) display defaultComponentName
        canonicalForm = CanonicalComponent{..}
        componentId = deterministicComponentId releaseId canonicalForm
        metadata = ComponentMetadata (ComponentCondition <$> condition)
        component = PackageComponent{..}
        dependencies = buildDependency package componentId <$> getDeps rawComponent
     in force (component, dependencies)

buildDependency :: Package -> ComponentId -> Cabal.Dependency -> ImportDependency
buildDependency package packageComponentId (Cabal.Dependency depName versionRange _) =
  force $
    let name = depName & unPackageName & pack & PackageName
        namespace = chooseNamespace name
        packageId = deterministicPackageId namespace name
        ownerId = package.ownerId
        createdAt = package.createdAt
        updatedAt = package.updatedAt
        status = UnknownPackage
        dependencyPackage = Package{..}
        requirement =
          Requirement
            { requirementId = deterministicRequirementId packageComponentId packageId
            , packageComponentId
            , packageId
            , requirement = display . prettyShow $ versionRange
            , metadata = RequirementMetadata{flag = Nothing}
            }
     in ImportDependency{package = dependencyPackage, requirement}

getRepoURL :: PackageName -> [Cabal.SourceRepo] -> Vector Text
getRepoURL _ [] = Vector.empty
getRepoURL _ (repo : _) = Vector.singleton $ display $ fromMaybe mempty (repo.repoLocation)

chooseNamespace :: PackageName -> Namespace
chooseNamespace name | Set.member name coreLibraries = Namespace "haskell"
chooseNamespace _ = Namespace "hackage"

extractTestedWith :: Vector (CompilerFlavor, VersionRange) -> Vector VersionRange
extractTestedWith testedWithVector =
  testedWithVector
    & Vector.filter (\(flavour, _) -> flavour == GHC)
    & Vector.filter (\(_, versionRange) -> any (`withinRange` versionRange) versionList)
    & Vector.map snd

getVersions :: Vector VersionRange -> Vector Version
getVersions supportedCompilers =
  foldMap
    (\version -> Vector.foldMap (\versionRange -> checkVersion version versionRange) supportedCompilers)
    versionList

checkVersion :: Version -> VersionRange -> Vector Version
checkVersion version versionRange =
  if version `withinRange` versionRange
    then Vector.singleton version
    else Vector.empty
