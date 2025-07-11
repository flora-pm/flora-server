-- |
-- Module: Flora.Import.Package
--
-- This module contains all the code to import Cabal packages into Flora. The import process
-- for a single package is divided in three consecutive steps:
--
--   1. The Cabal file is read from the file system and parsed into a 'GenericPackageDescription' from the Cabal package
--   2. Relevant data from the Cabal package is extracted and turned into an intermediate representation, 'ImportOutput'
--   3. This 'ImportOutput' is inserted (or more precisely upserted) into the database
--
-- We strive to keep step 2 deterministic and side-effect free, besides accessing the current time and logging.
-- We also want to keep the import procedure idempotent.
--
-- Packages can be imported in any order, even before their dependencies are known. When importing a package,
-- any dependency that isn't yet known will be imported as an "unknown package", as indicated by its status field.
-- If and when that package is fully imported later, we complete its data and change its status to "fully imported" without
-- altering its id.
module Flora.Import.Package
  ( versionList
  , loadContent
  , persistImportOutput
  , extractPackageDataFromCabal
  , chooseNamespace
  , loadJSONContent
  , persistHashes
  ) where

import Control.DeepSeq (force)
import Control.Exception ()
import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Compat.NonEmptySet qualified as NESet
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Fields.ParseResult
import Distribution.PackageDescription
  ( CondBranch (..)
  , CondTree (condTreeData)
  , Condition (CNot)
  , ConfVar
  , UnqualComponentName
  , allLibraries
  , unPackageName
  , unUnqualComponentName
  )
import Distribution.PackageDescription qualified as Cabal
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec qualified as Parsec
import Distribution.Types.Benchmark
import Distribution.Types.Dependency
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.PackageDescription ()
import Distribution.Types.TestSuite
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, withinRange)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version qualified as Version
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Concurrent
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.List (List)
import Log qualified
import Optics.Core
import RequireCallStack
import System.Exit (exitFailure)
import System.FilePath qualified as FilePath

import Flora.Environment.Config (PoolConfig (..))
import Flora.Environment.Env (FloraEnv (..))
import Flora.Import.Package.Types
import Flora.Import.Types
import Flora.Model.Category.Query as Query
import Flora.Model.Category.Types
import Flora.Model.Category.Update qualified as Update
import Flora.Model.Component.Types as Component
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release (deterministicReleaseId)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement
  ( Requirement (..)
  , deterministicRequirementId
  )
import Flora.Monad
import Flora.Normalise

versionList :: Set Version
versionList =
  Set.fromList
    [ Version.mkVersion [9, 12, 1]
    , Version.mkVersion [9, 10, 2]
    , Version.mkVersion [9, 10, 1]
    , Version.mkVersion [9, 8, 4]
    , Version.mkVersion [9, 8, 3]
    , Version.mkVersion [9, 8, 2]
    , Version.mkVersion [9, 8, 1]
    , Version.mkVersion [9, 6, 7]
    , Version.mkVersion [9, 6, 6]
    , Version.mkVersion [9, 6, 5]
    , Version.mkVersion [9, 6, 4]
    , Version.mkVersion [9, 6, 3]
    , Version.mkVersion [9, 6, 2]
    , Version.mkVersion [9, 6, 1]
    , Version.mkVersion [9, 4, 8]
    , Version.mkVersion [9, 4, 7]
    , Version.mkVersion [9, 4, 6]
    , Version.mkVersion [9, 4, 5]
    , Version.mkVersion [9, 4, 4]
    , Version.mkVersion [9, 4, 3]
    , Version.mkVersion [9, 4, 2]
    , Version.mkVersion [9, 4, 1]
    , Version.mkVersion [9, 2, 8]
    , Version.mkVersion [9, 2, 7]
    , Version.mkVersion [9, 2, 6]
    , Version.mkVersion [9, 2, 5]
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

loadContent :: (Error ImportError :> es, Log :> es) => FilePath -> BS.ByteString -> FloraM es GenericPackageDescription
loadContent = parseString parseGenericPackageDescription

loadJSONContent
  :: (IOE :> es, Log :> es, State (Map (Namespace, PackageName, Version) Text) :> es)
  => FilePath
  -> BS.ByteString
  -> Vector (Text, Set PackageName)
  -> FloraM es (Namespace, PackageName, Version, Target)
loadJSONContent path content indexPackages = do
  case getNameAndVersionFromPath path of
    Left (name, versionText) -> do
      Log.logAttention "Could not parse version" $
        object ["version" .= versionText, "package" .= name]
      error "Parse error"
    Right (name, version) -> do
      let packageName = PackageName name
      case chooseNamespace packageName indexPackages of
        Nothing -> undefined
        Just chosenNamespace -> do
          let field = "<repo>/package/" <> display packageName <> "-" <> display version <> ".tar.gz"
          mHashFromCache <- State.state $ \m ->
            case Map.lookup (chosenNamespace, packageName, version) m of
              Nothing -> (Nothing, m)
              Just (hash :: Text) -> (Just hash, Map.delete (chosenNamespace, packageName, version) m)
          case mHashFromCache of
            Nothing -> processJSONContent field chosenNamespace packageName version content
            Just hash -> do
              let target = Target (Hashes hash)
              pure (chosenNamespace, packageName, version, target)

processJSONContent
  :: (IOE :> es, Log :> es)
  => Text
  -> a
  -> b
  -> c
  -> BS.ByteString
  -> FloraM es (a, b, c, Target)
processJSONContent field namespace packageName version content = do
  let (mReleaseJSON :: Maybe ReleaseJSONFile) = Aeson.decodeStrict' content
  case mReleaseJSON of
    Nothing -> do
      Log.logAttention "Could not parse JSON" $
        object ["json" .= Text.decodeUtf8 content]
      liftIO exitFailure
    Just releaseJSON -> do
      let mTarget = KeyMap.lookup (Key.fromText field) releaseJSON.signed.targets
      case mTarget of
        Nothing -> do
          Log.logAttention ("Could not find field: " <> field) $
            object ["json" .= releaseJSON]
          liftIO exitFailure
        Just target -> do
          pure (namespace, packageName, version, target)

getNameAndVersionFromPath :: FilePath -> Either (Text, Text) (Text, Version)
getNameAndVersionFromPath path =
  case Text.split (== '/') $ Text.pack $ FilePath.takeDirectory path of
    [name, versionText] ->
      case Parsec.simpleParsec $ Text.unpack versionText of
        Nothing -> Left (name, versionText)
        Just version ->
          Right (name, version)
    _ -> Left ("", "")

parseString
  :: (Error ImportError :> es, Log :> es)
  => (BS.ByteString -> ParseResult a)
  -- ^ File contents to final value parser
  -> String
  -- ^ File name
  -> BS.ByteString
  -> FloraM es a
parseString parser name bs = do
  let (_warnings, result) = runParseResult (parser bs)
  case result of
    Right x -> pure x
    Left err -> do
      Log.logAttention_ (display $ show err)
      Error.throwError $ CabalFileCouldNotBeParsed name

-- | Persists an 'ImportOutput' to the database. An 'ImportOutput' can be obtained
--  by extracting relevant information from a Cabal file using 'extractPackageDataFromCabal'
persistImportOutput
  :: forall es
   . ( Concurrent :> es
     , DB :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => ImportOutput
  -> FloraM es ()
persistImportOutput (ImportOutput package categories release components) = State.modifyM $ \packageCache -> do
  persistPackage package.packageId
  if Set.member (package.namespace, package.name, release.version) packageCache
    then do
      Log.logInfo "Release already present" $
        object
          [ "namespace" .= package.namespace
          , "package" .= package.name
          , "version" .= release.version
          ]
      pure packageCache
    else do
      Update.upsertRelease package release
      env <- Reader.ask
      let componentsList = NE.toList $ fmap fst components
      let dependencies = foldMap snd components
      Update.upsertPackageComponents componentsList
      Concurrent.pooledForConcurrentlyN_ env.dbConfig.connections dependencies persistImportDependency
      unless (null dependencies) sanityCheck
      pure $ Set.insert (package.namespace, package.name, release.version) packageCache
  where
    persistPackage :: RequireCallStack => PackageId -> FloraM es ()
    persistPackage packageId = do
      Update.upsertPackage package
      categoriesByName <- catMaybes <$> traverse Query.getCategoryByName categories
      forM_
        categoriesByName
        (\c -> Update.addToCategoryByName packageId c.name)

    persistImportDependency :: RequireCallStack => ImportDependency -> FloraM es ()
    persistImportDependency dep = do
      Update.upsertPackage dep.package
      Update.upsertRequirement dep.requirement

    sanityCheck :: RequireCallStack => FloraM es ()
    sanityCheck = do
      dependencies <- Query.getAllRequirements release.releaseId
      when (Map.null dependencies) $ do
        Log.logAttention "No dependencies found after inserting release!" $
          object
            [ "namespace" .= package.namespace
            , "package" .= package.name
            , "version" .= release.version
            ]

persistHashes
  :: ( DB :> es
     , Log :> es
     , State (Map (Namespace, PackageName, Version) Text) :> es
     )
  => (Namespace, PackageName, Version, Target)
  -> FloraM es ()
persistHashes (namespace, packageName, version, target) = do
  mPackage <- Query.getPackageByNamespaceAndName namespace packageName
  case mPackage of
    Just package -> do
      mRelease <- Query.getReleaseByVersion package.packageId version
      case mRelease of
        Nothing -> do
          Log.logAttention "Release does not exist, saving the hash for later" $
            object
              [ "package" .= packageName
              , "namespace" .= namespace
              , "version" .= version
              , "hash" .= target.hashes.sha256
              ]
          State.modify (\m -> Map.insert (namespace, packageName, version) target.hashes.sha256 m)
        Just release -> do
          Update.setArchiveChecksum release.releaseId target.hashes.sha256
    Nothing -> do
      Log.logAttention "Package does not exist, saving the hash for later" $
        object
          [ "package" .= packageName
          , "namespace" .= namespace
          , "version" .= version
          , "hash" .= target.hashes.sha256
          ]
      State.modify (\m -> Map.insert (namespace, packageName, version) target.hashes.sha256 m)

-- | Transforms a 'GenericPackageDescription' from Cabal into an 'ImportOutput'
-- that can later be inserted into the database. This function produces stable, deterministic ids,
-- so it should be possible to extract and insert a single package many times in a row.
extractPackageDataFromCabal
  :: ( Error ImportError :> es
     , IOE :> es
     , Log :> es
     , RequireCallStack
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => Text
  -> Vector (Text, Set PackageName)
  -> UTCTime
  -> GenericPackageDescription
  -> FloraM es ImportOutput
extractPackageDataFromCabal repositoryName indexPackages uploadTime genericDesc = do
  let packageDesc = genericDesc.packageDescription
  let flags = Vector.fromList genericDesc.genPackageFlags
  let packageName = force $ packageDesc ^. #package % #pkgName % to unPackageName % to pack % to PackageName
  let buildType = Cabal.buildType genericDesc.packageDescription
  let packageVersion = force packageDesc.package.pkgVersion
  case chooseNamespace packageName indexPackages of
    Nothing -> do
      Log.logAttention "Could not select namespace" $
        object
          [ "package_name" .= display packageName
          , "index" .= repositoryName
          , "index_packages" .= indexPackages
          ]
      Error.throwError $ CouldNotSelectNamespace repositoryName packageName
    Just namespace -> do
      let packageId = deterministicPackageId namespace packageName
      let releaseId = deterministicReleaseId packageId packageVersion
      timestamp <- Time.currentTime
      let sourceRepos = getRepoURL packageName packageDesc.sourceRepos
      let rawCategoryField = packageDesc ^. #category % to fromShortText % to Text.pack
      let categoryList = fmap (Text.stripStart . Text.stripEnd) (Text.splitOn "," rawCategoryField)
      let categories = Maybe.mapMaybe normaliseCategory categoryList
      let package =
            Package
              { packageId
              , namespace
              , name = packageName
              , createdAt = timestamp
              , updatedAt = timestamp
              , status = FullyImportedPackage
              , deprecationInfo = Nothing
              }

      let release =
            Release
              { releaseId
              , packageId
              , version = packageVersion
              , archiveChecksum = Nothing
              , uploadedAt = Just uploadTime
              , createdAt = timestamp
              , updatedAt = timestamp
              , readme = Nothing
              , readmeStatus = NotImported
              , changelog = Nothing
              , changelogStatus = NotImported
              , repository = Just repositoryName
              , tarballRootHash = Nothing
              , tarballArchiveHash = Nothing
              , license = Cabal.license packageDesc
              , sourceRepos
              , homepage = Just $ display packageDesc.homepage
              , documentation = ""
              , bugTracker = Just $ display packageDesc.bugReports
              , maintainer = display packageDesc.maintainer
              , synopsis = display packageDesc.synopsis
              , description = display packageDesc.description
              , flags = ReleaseFlags flags
              , testedWith = getVersions . extractTestedWith . Vector.fromList $ packageDesc.testedWith
              , deprecated = Nothing
              , revisedAt = Nothing
              , buildType = buildType
              }

      let lib = extractLibrary package indexPackages release Nothing [] <$> allLibraries packageDesc
      let condLib = maybe [] (extractCondTree extractLibrary package indexPackages release Nothing) genericDesc.condLibrary
      let condSubLibs = extractCondTrees extractLibrary package indexPackages release genericDesc.condSubLibraries

      let foreignLibs = extractForeignLib package indexPackages release Nothing [] <$> packageDesc.foreignLibs
      let condForeignLibs = extractCondTrees extractForeignLib package indexPackages release genericDesc.condForeignLibs

      let executables = extractExecutable package indexPackages release Nothing [] <$> packageDesc.executables
      let condExecutables = extractCondTrees extractExecutable package indexPackages release genericDesc.condExecutables

      let testSuites = extractTestSuite package indexPackages release Nothing [] <$> packageDesc.testSuites
      let condTestSuites = extractCondTrees extractTestSuite package indexPackages release genericDesc.condTestSuites

      let benchmarks = extractBenchmark package indexPackages release Nothing [] <$> packageDesc.benchmarks
      let condBenchmarks = extractCondTrees extractBenchmark package indexPackages release genericDesc.condBenchmarks

      let components' =
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
      case NE.nonEmpty components' of
        Nothing -> do
          Log.logAttention "Empty dependencies" $ object ["package" .= package]
          extractPackageDataFromCabal repositoryName indexPackages uploadTime genericDesc
        Just components -> pure $ ImportOutput package categories release components

extractLibrary
  :: Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> Library
  -> (PackageComponent, List ImportDependency)
extractLibrary package =
  genericComponentExtractor
    Component.Library
    (^. #libName % to (getLibName package.name))
    (^. #libBuildInfo % #targetBuildDepends)
    package

getLibName :: PackageName -> LibraryName -> Text
getLibName pname LMainLibName = display pname
getLibName _ (LSubLibName lname) = Text.pack $ unUnqualComponentName lname

extractForeignLib
  :: Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> ForeignLib
  -> (PackageComponent, List ImportDependency)
extractForeignLib =
  genericComponentExtractor
    Component.ForeignLib
    (^. #foreignLibName % to unUnqualComponentName % to Text.pack)
    (^. #foreignLibBuildInfo % #targetBuildDepends)

extractExecutable
  :: Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> Executable
  -> (PackageComponent, List ImportDependency)
extractExecutable =
  genericComponentExtractor
    Component.Executable
    (^. #exeName % to unUnqualComponentName % to Text.pack)
    (^. #buildInfo % #targetBuildDepends)

extractTestSuite
  :: Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> TestSuite
  -> (PackageComponent, List ImportDependency)
extractTestSuite =
  genericComponentExtractor
    Component.TestSuite
    (^. #testName % to unUnqualComponentName % to Text.pack)
    (^. #testBuildInfo % #targetBuildDepends)

extractBenchmark
  :: Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> Benchmark
  -> (PackageComponent, List ImportDependency)
extractBenchmark =
  genericComponentExtractor
    Component.Benchmark
    (^. #benchmarkName % to unUnqualComponentName % to Text.pack)
    (^. #benchmarkBuildInfo % #targetBuildDepends)

-- | Traverses the provided 'CondTree' and applies the given 'ComponentExtractor'
--  to every node, returning a list of '(PackageComponent, List ImportDependency)'
extractCondTree
  :: (Package -> Vector (Text, Set PackageName) -> Release -> Maybe UnqualComponentName -> List (Condition ConfVar) -> component -> (PackageComponent, List ImportDependency))
  -> Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> CondTree ConfVar (List Dependency) component
  -> List (PackageComponent, List ImportDependency)
extractCondTree extractor package indexPackages release defaultComponentName = go []
  where
    go cond tree =
      let treeComponent = extractor package indexPackages release defaultComponentName cond tree.condTreeData
          treeSubComponents = tree.condTreeComponents >>= extractBranch
       in treeComponent : treeSubComponents
    extractBranch CondBranch{condBranchCondition, condBranchIfTrue, condBranchIfFalse} =
      let condIfTrueComponents = go [condBranchCondition] condBranchIfTrue
          condIfFalseComponents = maybe [] (go [CNot condBranchCondition]) condBranchIfFalse
       in condIfTrueComponents <> condIfFalseComponents

-- | Cabal often models conditional components as a list of 'CondTree' associated with an 'UnqualComponentName'.
--  This function builds upon 'extractCondTree' to make it easier to extract fields such as 'condExecutables', 'condTestSuites' etc.
--  from a 'GenericPackageDescription'
extractCondTrees
  :: (Package -> Vector (Text, Set PackageName) -> Release -> Maybe UnqualComponentName -> List (Condition ConfVar) -> component -> (PackageComponent, List ImportDependency))
  -> Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> List (UnqualComponentName, CondTree ConfVar (List Dependency) component)
  -> List (PackageComponent, List ImportDependency)
extractCondTrees extractor package indexPackages release trees =
  trees >>= \case (name, tree) -> extractCondTree extractor package indexPackages release (Just name) tree

genericComponentExtractor
  :: forall component
   . ComponentType
  -> (component -> Text)
  -- ^ Extract name from component
  -> (component -> List Dependency)
  -- ^ Extract dependencies
  -> Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> List (Condition ConfVar)
  -> component
  -> (PackageComponent, List ImportDependency)
genericComponentExtractor
  componentType
  getName
  getDeps
  package
  indexPackages
  release
  defaultComponentName
  condition
  rawComponent =
    let releaseId = release.releaseId
        componentName = maybe (getName rawComponent) display defaultComponentName
        canonicalForm = CanonicalComponent componentName componentType
        componentId = deterministicComponentId releaseId canonicalForm
        metadata = ComponentMetadata (ComponentCondition <$> condition)
        component = PackageComponent componentId releaseId canonicalForm metadata
        dependencies = mapMaybe (buildDependency package indexPackages componentId) (getDeps rawComponent)
     in (component, dependencies)

buildDependency
  :: Package
  -> Vector (Text, Set PackageName)
  -> ComponentId
  -> Cabal.Dependency
  -> Maybe ImportDependency
buildDependency package indexPackages packageComponentId (Cabal.Dependency depName versionRange libs) = do
  let name = depName & unPackageName & pack & PackageName
  namespace <- chooseNamespace name indexPackages
  let packageId = deterministicPackageId namespace name
      createdAt = package.createdAt
      updatedAt = package.updatedAt
      status = UnknownPackage
      deprecationInfo = Nothing
      dependencyPackage = Package packageId namespace name createdAt updatedAt status deprecationInfo
      requirement =
        Requirement
          { requirementId = deterministicRequirementId packageComponentId packageId
          , packageComponentId
          , packageId
          , requirement = display versionRange
          , components = Vector.fromList $ NESet.toList $ NESet.map (getLibName name) libs
          }
   in Just (ImportDependency{package = dependencyPackage, requirement})

getRepoURL :: PackageName -> List Cabal.SourceRepo -> Vector Text
getRepoURL _ [] = Vector.empty
getRepoURL _ (repo : _) = Vector.singleton $ display $ fromMaybe mempty repo.repoLocation

chooseNamespace
  :: PackageName
  -> Vector (Text, Set PackageName)
  -> Maybe Namespace
chooseNamespace name packages =
  let result = Vector.mapMaybe (\(repo, indexPackages) -> isPackageInSet name (repo, indexPackages)) packages
   in case Vector.uncons result of
        Just (found, _) -> Just found
        Nothing -> Nothing

isPackageInSet :: PackageName -> (Text, Set PackageName) -> Maybe Namespace
isPackageInSet packageName (indexName, indexPackages)
  | packageName `Set.member` indexPackages = Just (Namespace indexName)
  | otherwise = Nothing

extractTestedWith :: Vector (CompilerFlavor, VersionRange) -> Vector VersionRange
extractTestedWith testedWithVector =
  testedWithVector
    & Vector.filter (\(flavour, _) -> flavour == GHC)
    & Vector.filter (\(_, versionRange) -> any (`withinRange` versionRange) versionList)
    & Vector.map snd

getVersions :: Vector VersionRange -> Vector Version
getVersions supportedCompilers =
  foldMap
    (\version -> Vector.foldMap (checkVersion version) supportedCompilers)
    versionList
checkVersion :: Version -> VersionRange -> Vector Version
checkVersion version versionRange =
  if version `withinRange` versionRange
    then Vector.singleton version
    else Vector.empty
