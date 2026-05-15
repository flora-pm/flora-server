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
  , flattenCondTree
  ) where

import Control.Applicative ((<|>))
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
import Distribution.Compat.Lens qualified as L
import Distribution.Compat.NonEmptySet qualified as NESet
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Fields.ParseResult
import Distribution.PackageDescription hiding (PackageId, PackageName)
import Distribution.PackageDescription qualified as Cabal
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec qualified as Parsec
import Distribution.Types.BuildInfo.Lens qualified as L
import Distribution.Types.PackageDescription ()
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version (Version, VersionRange, withinRange)
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
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Types
import Flora.Model.PackageUploader.Update qualified as Update
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

flattenCondTree
  :: CondTree ConfVar c component
  -> [(Maybe (Condition ConfVar), component)]
flattenCondTree = flattenCondTreeAcc Nothing

flattenCondTreeAcc
  :: Maybe (Condition ConfVar)
  -- ^ Condition accumulator.
  -> CondTree ConfVar c component
  -> [(Maybe (Condition ConfVar), component)]
flattenCondTreeAcc condAcc (Cabal.CondNode comp _ components) =
  let components' = flattenCondBranchAcc condAcc =<< components
   in (condAcc, comp) : components'

flattenCondBranchAcc
  :: Maybe (Condition ConfVar)
  -- ^ Condition accumulator.
  -> CondBranch ConfVar c component
  -> [(Maybe (Condition ConfVar), component)]
flattenCondBranchAcc condAcc (CondBranch cond ifTrue maybeIfFalse) =
  let ifTrue' = flattenCondTreeAcc ((`cAnd` cond) <$> condAcc <|> Just cond) ifTrue
      ifFalse' = flattenCondTreeAcc ((`cAnd` cNot cond) <$> condAcc <|> Just (cNot cond)) =<< maybeToList maybeIfFalse
   in ifTrue' ++ ifFalse'

versionList :: Set Version
versionList =
  Set.fromList
    [ Version.mkVersion [9, 12, 2]
    , Version.mkVersion [9, 12, 1]
    , Version.mkVersion [9, 10, 3]
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
      let dependencyPackages = fmap (.package) dependencies
      Update.upsertPackageComponents componentsList
      Concurrent.pooledForConcurrentlyN_ env.dbConfig.connections dependencyPackages Update.upsertPackage
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
      Log.logInfo "Inserting requirement" $
        object
          [ "dependent_namespace" .= display package.namespace
          , "dependent_name" .= display package.name
          , "dependent_id" .= display package.packageId
          , "dependency_namespace" .= display dep.package.namespace
          , "dependency_name" .= display dep.package.name
          , "dependency_id" .= display dep.package.packageId
          ]
      Update.upsertRequirement dep.requirement

    sanityCheck :: RequireCallStack => FloraM es ()
    sanityCheck = do
      dependencies <- Query.getAllRequirements release.releaseId
      when (Map.null dependencies) $ do
        Log.logAttention "No dependencies found after inserting release!" $
          object
            [ "namespace" .= package.namespace
            , "package" .= package.name
            , "package_id" .= display package.packageId
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
  :: ( DB :> es
     , Error ImportError :> es
     , IOE :> es
     , Log :> es
     , RequireCallStack
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => PackageIndex
  -> Vector (Text, Set PackageName)
  -> UTCTime
  -> Maybe Text
  -> GenericPackageDescription
  -> FloraM es ImportOutput
extractPackageDataFromCabal packageIndex indexPackages uploadTime mUsername genericDesc = do
  let packageDesc = genericDesc.packageDescription
  let flags = Vector.fromList genericDesc.genPackageFlags
  let packageName = force $ packageDesc ^. #package % #pkgName % to unPackageName % to pack % to PackageName
  let packageBuildType = Cabal.buildType genericDesc.packageDescription
  let packageVersion = force packageDesc.package.pkgVersion
  case chooseNamespace packageName indexPackages of
    Nothing -> do
      Log.logAttention "Could not select namespace" $
        object
          [ "package_name" .= display packageName
          , "index" .= packageIndex.repository
          , "index_packages" .= indexPackages
          ]
      Error.throwError $ CouldNotSelectNamespace packageIndex.repository packageName
    Just namespace -> do
      let packageId = deterministicPackageId namespace packageName
      let releaseId = deterministicReleaseId packageId packageVersion
      timestamp <- Time.currentTime
      let sourceRepos = getRepoURL packageName packageDesc.sourceRepos
      let rawCategoryField = packageDesc ^. #category % to fromShortText % to Text.pack
      let categoryList = fmap (Text.stripStart . Text.stripEnd) (Text.splitOn "," rawCategoryField)
      let categories = Maybe.mapMaybe normaliseCategory categoryList
      mPackageUploaderId <- determinePackageUploaderId mUsername packageIndex.packageIndexId
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
              , repository = Just packageIndex.repository
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
              , buildType = packageBuildType
              , uploaderId = mPackageUploaderId
              }

      let extractCondTreeComponent' :: L.HasBuildInfo component => ComponentType -> Text -> CondTree ConfVar c component -> (PackageComponent, List ImportDependency)
          extractCondTreeComponent' = extractCondTreeComponent package indexPackages release

          lib :: [(PackageComponent, [ImportDependency])]
          lib = extractCondTreeComponent' Component.Library (display package.name) <$> maybeToList genericDesc.condLibrary

          subLibs :: [(PackageComponent, [ImportDependency])]
          subLibs =
            (\(compName, subLibrary) -> extractCondTreeComponent' Component.Library (display compName) subLibrary)
              <$> genericDesc.condSubLibraries

          foreignLibs :: [(PackageComponent, [ImportDependency])]
          foreignLibs =
            (\(compName, fLib) -> extractCondTreeComponent' Component.ForeignLib (display compName) fLib)
              <$> genericDesc.condForeignLibs

          executables :: [(PackageComponent, [ImportDependency])]
          executables =
            (\(compName, exe) -> extractCondTreeComponent' Component.Executable (display compName) exe)
              <$> genericDesc.condExecutables

          testSuites :: [(PackageComponent, [ImportDependency])]
          testSuites =
            (\(compName, testSuite) -> extractCondTreeComponent' Component.TestSuite (display compName) testSuite)
              <$> genericDesc.condTestSuites

          benchmarks :: [(PackageComponent, [ImportDependency])]
          benchmarks =
            (\(compName, benchmark) -> extractCondTreeComponent' Component.Benchmark (display compName) benchmark)
              <$> genericDesc.condBenchmarks

      let components' =
            lib
              <> subLibs
              <> foreignLibs
              <> executables
              <> testSuites
              <> benchmarks
      case NE.nonEmpty components' of
        Nothing -> do
          Log.logAttention "Empty dependencies" $ object ["package" .= package]
          extractPackageDataFromCabal packageIndex indexPackages uploadTime mUsername genericDesc
        Just components -> pure $ ImportOutput package categories release components

determinePackageUploaderId :: (DB :> es, IOE :> es) => Maybe Text -> PackageIndexId -> Eff es (Maybe PackageUploaderId)
determinePackageUploaderId Nothing _ = pure Nothing
determinePackageUploaderId (Just username) packageIndexId = Just <$> Update.getOrInsertPackageUploader username packageIndexId

extractCondTreeComponent
  :: L.HasBuildInfo component
  => Package
  -> Vector (Text, Set PackageName)
  -> Release
  -> ComponentType
  -> Text
  -- ^ component name
  -> CondTree ConfVar c component
  -> (PackageComponent, List ImportDependency)
extractCondTreeComponent
  package
  indexPackages
  release
  componentType
  componentName
  conditionalComp =
    ( mkPackageComponent componentType componentName release
    , mkImportDependencies
        package
        indexPackages
        (mkComponentId componentType componentName release)
        conditionalComp
    )

getLibName :: PackageName -> LibraryName -> Text
getLibName pname LMainLibName = display pname
getLibName _ (LSubLibName lname) = Text.pack $ unUnqualComponentName lname

mkPackageComponent :: ComponentType -> Text -> Release -> PackageComponent
mkPackageComponent componentType componentName release =
  let releaseId = release.releaseId
      canonicalForm = CanonicalComponent componentName componentType
      componentId = deterministicComponentId releaseId canonicalForm
   in PackageComponent componentId releaseId canonicalForm

mkImportDependency
  :: Package
  -> Vector (Text, Set PackageName)
  -> ComponentId
  -> Maybe (Condition ConfVar)
  -> Cabal.Dependency
  -> Maybe ImportDependency
mkImportDependency package indexPackages packageComponentId cond (Cabal.Dependency depName versionRange libs) = do
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
          , condition = cond
          }
   in Just (ImportDependency{package = dependencyPackage, requirement})

mkComponentId
  :: ComponentType
  -> Text
  -> Release
  -> ComponentId
mkComponentId componentType componentName release =
  let canonicalForm = CanonicalComponent componentName componentType
      releaseId = release.releaseId
   in deterministicComponentId releaseId canonicalForm

mkImportDependencies
  :: L.HasBuildInfo component
  => Package
  -> Vector (Text, Set PackageName)
  -> ComponentId
  -> CondTree ConfVar c component
  -> [ImportDependency]
mkImportDependencies package indexPackages packageComponentId comp =
  let conditionalDeps :: [(Maybe (Condition ConfVar), [Dependency])]
      conditionalDeps = fmap (L.view L.targetBuildDepends) <$> flattenCondTree comp
      mkImportDependency' = mkImportDependency package indexPackages packageComponentId
   in (\(cond, deps) -> mkImportDependency' cond `mapMaybe` deps) =<< conditionalDeps

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
