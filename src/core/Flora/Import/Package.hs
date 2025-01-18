{-# LANGUAGE MultiWayIf #-}

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
  ( coreLibraries
  , versionList
  , loadContent
  , persistImportOutput
  , extractPackageDataFromCabal
  , chooseNamespace
  , loadJSONContent
  , persistHashes
  ) where

import Control.DeepSeq (force)
import Control.Exception
import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Compat.NonEmptySet (toList)
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
import Distribution.PackageDescription qualified as Cabal hiding (PackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec qualified as Parsec
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
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, withinRange)
import Distribution.Version qualified as Version
import Effectful
import Effectful.Log (Log)
import Effectful.Poolboy (Poolboy)
import Effectful.Poolboy qualified as Poolboy
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.List (List)
import Log qualified
import Optics.Core
import System.Exit (exitFailure)
import System.FilePath qualified as FilePath

import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Flora.Import.Package.Types
import Flora.Import.Types
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
import Flora.Model.User
import Flora.Normalise

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

loadContent :: Log :> es => FilePath -> BS.ByteString -> Eff es GenericPackageDescription
loadContent = parseString parseGenericPackageDescription

loadJSONContent
  :: (Log :> es, IOE :> es, State (Map (Namespace, PackageName, Version) Text) :> es)
  => FilePath
  -> BS.ByteString
  -> (Text, Set PackageName)
  -> Eff es (Namespace, PackageName, Version, Target)
loadJSONContent path content (repositoryName, repositoryPackages) = do
  case getNameAndVersionFromPath path of
    Left (name, versionText) -> do
      Log.logAttention "Could not parse version" $
        object ["version" .= versionText, "package" .= name]
      error "Parse error"
    Right (name, version) -> do
      let packageName = PackageName name
      let chosenNamespace = chooseNamespace packageName (repositoryName, repositoryPackages)
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
  :: (Log :> es, IOE :> es)
  => Text
  -> a
  -> b
  -> c
  -> BS.ByteString
  -> Eff es (a, b, c, Target)
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
  :: Log :> es
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

-- | Persists an 'ImportOutput' to the database. An 'ImportOutput' can be obtained
--  by extracting relevant information from a Cabal file using 'extractPackageDataFromCabal'
persistImportOutput
  :: forall es
   . (State (Set (Namespace, PackageName, Version)) :> es, Log :> es, Poolboy :> es, DB :> es, IOE :> es)
  => ImportOutput
  -> Eff es ()
persistImportOutput (ImportOutput package categories release components) = State.modifyM $ \packageCache -> do
  Log.logInfo "Persisting package" $
    object
      [ "package_name" .= packageName
      , "version" .= display release.version
      ]
  persistPackage
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
      Update.upsertRelease release
      parallelRun persistComponent components
      pure $ Set.insert (package.namespace, package.name, release.version) packageCache
  where
    parallelRun :: Foldable t => (a -> Eff es ()) -> t a -> Eff es ()
    parallelRun f xs = forM_ xs (Poolboy.enqueue . f)
    packageName = display package.namespace <> "/" <> display package.name
    persistPackage = do
      let packageId = package.packageId
      Update.upsertPackage package
      forM_ categories (\case (_, name, _) -> Update.addToCategoryByName packageId name)

    persistComponent :: (PackageComponent, List ImportDependency) -> Eff es ()
    persistComponent (packageComponent, deps) = do
      Log.logInfo
        "Persisting component"
        $ object
          [ "component" .= display packageComponent.canonicalForm
          , "number_of_dependencies" .= display (length deps)
          ]
      Update.upsertPackageComponent packageComponent
      parallelRun persistImportDependency deps

    persistImportDependency :: ImportDependency -> Eff es ()
    persistImportDependency dep = do
      Update.upsertPackage dep.package
      Update.upsertRequirement dep.requirement

persistHashes
  :: ( DB :> es
     , Log :> es
     , State (Map (Namespace, PackageName, Version) Text) :> es
     )
  => (Namespace, PackageName, Version, Target)
  -> Eff es ()
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
  :: ( IOE :> es
     , Time :> es
     , Log :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     )
  => UserId
  -> (Text, Set PackageName)
  -> UTCTime
  -> GenericPackageDescription
  -> Eff es ImportOutput
extractPackageDataFromCabal userId repository@(repositoryName, repositoryPackages) uploadTime genericDesc = do
  let packageDesc = genericDesc.packageDescription
  let flags = Vector.fromList genericDesc.genPackageFlags
  let packageName = force $ packageDesc ^. #package % #pkgName % to unPackageName % to pack % to PackageName
  let buildType = Cabal.buildType genericDesc.packageDescription
  let packageVersion = force packageDesc.package.pkgVersion
  let namespace = chooseNamespace packageName repository
  let packageId = deterministicPackageId namespace packageName
  let releaseId = deterministicReleaseId packageId packageVersion
  timestamp <- Time.currentTime
  let sourceRepos = getRepoURL packageName packageDesc.sourceRepos
  let categories = floraCategories
  let package =
        Package
          { packageId
          , namespace
          , name = packageName
          , ownerId = userId
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

  let lib = extractLibrary package (repositoryName, repositoryPackages) release Nothing [] <$> allLibraries packageDesc
  let condLib = maybe [] (extractCondTree extractLibrary package (repositoryName, repositoryPackages) release Nothing) genericDesc.condLibrary
  let condSubLibs = extractCondTrees extractLibrary package (repositoryName, repositoryPackages) release genericDesc.condSubLibraries

  let foreignLibs = extractForeignLib package (repositoryName, repositoryPackages) release Nothing [] <$> packageDesc.foreignLibs
  let condForeignLibs = extractCondTrees extractForeignLib package (repositoryName, repositoryPackages) release genericDesc.condForeignLibs

  let executables = extractExecutable package (repositoryName, repositoryPackages) release Nothing [] <$> packageDesc.executables
  let condExecutables = extractCondTrees extractExecutable package (repositoryName, repositoryPackages) release genericDesc.condExecutables

  let testSuites = extractTestSuite package (repositoryName, repositoryPackages) release Nothing [] <$> packageDesc.testSuites
  let condTestSuites = extractCondTrees extractTestSuite package (repositoryName, repositoryPackages) release genericDesc.condTestSuites

  let benchmarks = extractBenchmark package (repositoryName, repositoryPackages) release Nothing [] <$> packageDesc.benchmarks
  let condBenchmarks = extractCondTrees extractBenchmark package (repositoryName, repositoryPackages) release genericDesc.condBenchmarks

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
      extractPackageDataFromCabal userId (repositoryName, repositoryPackages) uploadTime genericDesc
    Just components -> pure ImportOutput{..}

extractLibrary
  :: Package
  -> (Text, Set PackageName)
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
  -> (Text, Set PackageName)
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
  -> (Text, Set PackageName)
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
  -> (Text, Set PackageName)
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
  -> (Text, Set PackageName)
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
  :: (Package -> (Text, Set PackageName) -> Release -> Maybe UnqualComponentName -> List (Condition ConfVar) -> component -> (PackageComponent, List ImportDependency))
  -> Package
  -> (Text, Set PackageName)
  -> Release
  -> Maybe UnqualComponentName
  -> CondTree ConfVar (List Dependency) component
  -> List (PackageComponent, List ImportDependency)
extractCondTree extractor package repository release defaultComponentName = go []
  where
    go cond tree =
      let treeComponent = extractor package repository release defaultComponentName cond tree.condTreeData
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
  :: (Package -> (Text, Set PackageName) -> Release -> Maybe UnqualComponentName -> List (Condition ConfVar) -> component -> (PackageComponent, List ImportDependency))
  -> Package
  -> (Text, Set PackageName)
  -> Release
  -> List (UnqualComponentName, CondTree ConfVar (List Dependency) component)
  -> List (PackageComponent, List ImportDependency)
extractCondTrees extractor package repository release trees =
  trees >>= \case (name, tree) -> extractCondTree extractor package repository release (Just name) tree

genericComponentExtractor
  :: forall component
   . ()
  => ComponentType
  -> (component -> Text)
  -- ^ Extract name from component
  -> (component -> List Dependency)
  -- ^ Extract dependencies
  -> Package
  -> (Text, Set PackageName)
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
  repository
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
        dependencies = buildDependency package repository componentId <$> getDeps rawComponent
     in (component, dependencies)

buildDependency
  :: Package
  -> (Text, Set PackageName)
  -> ComponentId
  -> Cabal.Dependency
  -> ImportDependency
buildDependency package repository packageComponentId (Cabal.Dependency depName versionRange libs) =
  let name = depName & unPackageName & pack & PackageName
      namespace = chooseNamespace name repository
      packageId = deterministicPackageId namespace name
      ownerId = package.ownerId
      createdAt = package.createdAt
      updatedAt = package.updatedAt
      status = UnknownPackage
      deprecationInfo = Nothing
      dependencyPackage = Package{..}
      requirement =
        Requirement
          { requirementId = deterministicRequirementId packageComponentId packageId
          , packageComponentId
          , packageId
          , requirement = display . prettyShow $ versionRange
          , components = fmap (getLibName name) . Vector.fromList $ toList libs
          }
   in ImportDependency{package = dependencyPackage, requirement}

getRepoURL :: PackageName -> List Cabal.SourceRepo -> Vector Text
getRepoURL _ [] = Vector.empty
getRepoURL _ (repo : _) = Vector.singleton $ display $ fromMaybe mempty repo.repoLocation

chooseNamespace :: PackageName -> (Text, Set PackageName) -> Namespace
chooseNamespace name (repo, repositoryPackages) =
  if
    | name `Set.member` coreLibraries -> Namespace "haskell"
    | name `Set.member` repositoryPackages -> Namespace repo
    | otherwise -> Namespace "hackage"

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
