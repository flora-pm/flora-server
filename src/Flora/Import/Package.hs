{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.Import.Package where

import Control.Concurrent
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Transact
import Debug.Pretty.Simple
import Distribution.PackageDescription
  ( PackageDescription
  , UnqualComponentName
  , allLibraries
  , benchmarks
  , depPkgName
  , executables
  , foreignLibs
  , library
  , subLibraries
  , targetBuildDepends
  , testSuites
  , unUnqualComponentName
  )
import qualified Distribution.PackageDescription as Cabal hiding (PackageName)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty
import Distribution.Types.Benchmark
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.TestSuite
import Distribution.Types.Version
import Optics.Core
import qualified System.Directory as System
import System.Process.Typed

import Control.Monad.Trans.Control (MonadTransControl)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import qualified Distribution.Utils.ShortText as Cabal
import Flora.Import.Categories.Tuning (UserPackageCategory (..))
import Flora.Import.Types
import Flora.Model.Package.Component as Component
import Flora.Model.Package.Orphans ()
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Package.Types
import Flora.Model.Release
import qualified Flora.Model.Release.Query as Query
import Flora.Model.Requirement
  ( Requirement (..)
  , RequirementId (..)
  , RequirementMetadata (..)
  , flag
  )
import Flora.Model.User
import qualified Flora.Publish as Update
import Log (LogT, MonadLog)

{- | This tuple represents the package that depends on any associated dependency/requirement.
 It is used in the recursive loading of Cabal files
-}
type DependentName = (Namespace, PackageName)

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
    , PackageName "directory"
    , PackageName "deepseq"
    , PackageName "ghc-bignum"
    , PackageName "ghc-boot-th"
    , PackageName "ghc-prim"
    , PackageName "integer-simple"
    , PackageName "integer-gmp"
    , PackageName "mtl"
    , PackageName "parsec"
    , PackageName "process"
    , PackageName "rts"
    , PackageName "stm"
    , PackageName "text"
    , PackageName "transformers"
    , PackageName "unix"
    ]

importPackage ::
  (MonadIO m) =>
  -- | The UserId of the stand-in user for Hackage, for instance.
  UserId ->
  -- | Name of the package and of the .cabal file
  PackageName ->
  -- | Directory where to find the .cabal files
  FilePath ->
  DBT m (Vector Package)
importPackage userId packageName directory = do
  packageDeps <- buildSlices <$> importPackageDeps packageName directory
  when (packageName == PackageName "Cabal") $ liftIO $ print packageDeps
  pTraceShowM packageDeps
  Vector.forM
    packageDeps
    ( \packageSet ->
        let package = Set.findMin packageSet
            cabalPath = directory <> T.unpack (display package) <> ".cabal"
         in importCabal userId package cabalPath directory
    )

-- 1. Load the .cabal file at the given path
-- 2. Translate it to a GenericPackageDescription
-- 3. Extract a 'Package', see if it already exists
-- 4. Extract a 'Release'
-- 5. Extract multiple 'PackageComponent's
-- 6. Extract multiple 'Requirement's
-- 7. Insert everything
importCabal ::
  (MonadIO m) =>
  -- | The UserId of the stand-in user for Hackage, for instance.
  UserId ->
  -- | Name of the package and of the .cabal file
  PackageName ->
  -- | Path to the Cabal file
  FilePath ->
  -- | Directory where to find the .cabal files
  FilePath ->
  DBT m Package
importCabal userId packageName cabalFile directory = do
  let namespace = if Set.member packageName coreLibraries then Namespace "haskell" else Namespace "hackage"
  genDesc <- liftIO $ loadFile cabalFile
  result <- runExceptT $ do
    package <- importPackageData userId genDesc namespace packageName
    liftIO $ T.putStrLn $ "[+] Package " <> display packageName <> " in importCabal"
    release <- lift $ importRelease genDesc namespace packageName package
    componentsAndRequirements <-
      extractComponents
        userId
        directory
        (namespace, packageName)
        (flattenPackageDescription genDesc)
        (release ^. #releaseId)
        (package ^. #name)
    let components = fmap fst componentsAndRequirements
    let requirements = foldMap snd componentsAndRequirements
    pure (package, release, components, requirements)
  case result of
    Left err -> error $ "Encountered error during import: " <> show err
    Right (package, release, components, requirements) -> do
      let rawCategoryField = T.pack $ Cabal.fromShortText $ genDesc ^. (#packageDescription % #category)
      let categoryList = fmap (UserPackageCategory . T.stripStart) (T.splitOn "," rawCategoryField)
      Update.publishPackage requirements components release categoryList package

importPackageData :: (MonadIO m) => UserId -> GenericPackageDescription -> Namespace -> PackageName -> ExceptT ImportError (DBT m) Package
importPackageData userId genDesc namespace packageName = do
  result <- lift $ Query.getHaskellOrHackagePackage packageName
  case result of
    Nothing -> do
      logImportMessage (namespace, packageName) $
        "\"" <> display packageName <> "\" could not be found in the database."
      logImportMessage (namespace, packageName) $
        "Creating new package for " <> display packageName <> "."
      cabalToPackage userId (genDesc ^. #packageDescription) namespace packageName
    Just package -> do
      logImportMessage (namespace, packageName) $
        "Package " <> display packageName <> "exists."
      pure package

importRelease :: (MonadIO m) => GenericPackageDescription -> Namespace -> PackageName -> Package -> DBT m Release
importRelease genDesc namespace packageName package = do
  result <- Query.getReleaseByVersion (package ^. #packageId) ((genDesc ^. #packageDescription) ^. (#package % #pkgVersion))
  case result of
    Nothing -> do
      r <-
        createRelease
          (package ^. #packageId)
          ((genDesc ^. #packageDescription) ^. (#package % #pkgVersion))
      logImportMessage (namespace, packageName) $
        "Creating Release "
          <> display (r ^. #releaseId)
          <> " for package "
          <> display (package ^. #name)
      pure r
    Just release -> do
      logImportMessage (namespace, packageName) $
        "Release found: releaseId: " <> display (release ^. #releaseId) <> " / packageId: "
      pure release

importPackageDeps :: (MonadIO m) => PackageName -> FilePath -> DBT m (Map PackageName (Set PackageName))
importPackageDeps pName directory = do
  genDesc <- liftIO $ loadFile (directory <> T.unpack (display pName) <> ".cabal")
  let dependencies =
        concat $
          maybeToList $
            flattenPackageDescription genDesc ^. #library ^? _Just % #libBuildInfo % #targetBuildDepends
  let names = Set.fromList $ fmap (PackageName . T.pack . Cabal.unPackageName . depPkgName) dependencies
  let depsMap = Map.singleton pName names
  go names depsMap
  where
    go :: (MonadIO m) => Set PackageName -> Map PackageName (Set PackageName) -> DBT m (Map PackageName (Set PackageName))
    go control acc =
      case Set.lookupMin control of
        Nothing -> pure acc
        Just p ->
          case Map.lookup p acc of
            Just _ -> go (Set.deleteMin control) acc
            Nothing -> do
              genDesc <- loadFile (directory <> T.unpack (display p) <> ".cabal")
              let dependencies =
                    filter (\d -> Cabal.depPkgName d /= "unbuildable") $
                      concat $
                        maybeToList $
                          flattenPackageDescription genDesc ^. #library ^? _Just % #libBuildInfo % #targetBuildDepends
              let names = Set.fromList $ fmap (PackageName . T.pack . Cabal.unPackageName . depPkgName) dependencies
              let names' = Set.filter (\x -> isNothing . Map.lookup x $ acc) names
              go (Set.union (Set.deleteMin control) names') (Map.insert p names acc)

-- What we plan to do
--
-- 2. Find all packages that are _sources_ (namely, lack any dependencies of
--    their own)
-- 3. For all slices i from 0 onwards, do the following repeatedly:
--    a. Find all packages whose dependencies are in slices i - 1, i -2, ... 0
--       only.
--    b. Put all such packages into slice i.
--    c. If no more packages remain unassigned, stop; otherwise, continue to
--       slice i+1.
buildSlices :: Map PackageName (Set PackageName) -> Vector (Set PackageName)
buildSlices deps =
  -- deps is read-only
  let remaining = Map.keysSet deps -- modifies as we iterate
      done = mempty -- modifies as we iterate
   in Vector.unfoldr go (done, remaining)
  where
    go :: (Set PackageName, Set PackageName) -> Maybe (Set PackageName, (Set PackageName, Set PackageName))
    go (done, remaining)
      | Set.null remaining = Nothing
      | otherwise =
          let (currentSlice, newRemaining) = separateSlice done remaining
              newDone = Set.union currentSlice done
           in pure (currentSlice, (newDone, newRemaining))

    separateSlice :: Set PackageName -> Set PackageName -> (Set PackageName, Set PackageName)
    separateSlice done = Set.foldl' (go2 done) (mempty, mempty)

    go2 :: Set PackageName -> (Set PackageName, Set PackageName) -> PackageName -> (Set PackageName, Set PackageName)
    go2 done acc@(workingSlice, workingRemaining) package =
      case Map.lookup package deps of
        Nothing -> acc -- impossible
        Just deps' ->
          if deps' `Set.isSubsetOf` done
            then (Set.insert package workingSlice, workingRemaining)
            else (workingSlice, Set.insert package workingRemaining)

loadFile :: (MonadIO m) => FilePath -> m GenericPackageDescription
loadFile path = do
  liftIO $ T.putStrLn $ "Looking up cabal file " <> display path
  liftIO $ fromJust . parseGenericPackageDescriptionMaybe <$> B.readFile path

extractComponents ::
  (MonadIO m) =>
  UserId ->
  FilePath ->
  DependentName ->
  -- | Description from the parsed .cabal file
  PackageDescription ->
  -- | Id of the release we're inserting
  ReleaseId ->
  -- | Name of the package to which the release belongs
  PackageName ->
  ExceptT ImportError (DBT m) [(PackageComponent, [Requirement])]
extractComponents userId directory dependentName pkgDesc releaseId packageName = do
  mainLib <- traverse (extractFromLib userId directory dependentName releaseId packageName) (maybeToList $ library pkgDesc)
  let subLibComps = [] -- traverse (extractFromLib userId directory dependentName releaseId packageName) (subLibraries pkgDesc)
  let executableComps = [] -- traverse (extractFromExecutable userId directory dependentName releaseId) (executables pkgDesc)
  let testSuiteComps = [] -- traverse (extractFromTestSuite  userId directory dependentName releaseId) (testSuites pkgDesc)
  let benchmarkComps = [] -- traverse (extractFromBenchmark  userId directory dependentName releaseId) (benchmarks pkgDesc)
  let foreignLibComps = [] -- traverse (extractFromforeignLib userId directory dependentName releaseId) (foreignLibs pkgDesc)
  pure $ mainLib <> subLibComps <> executableComps <> testSuiteComps <> benchmarkComps <> foreignLibComps

extractFromLib ::
  (MonadIO m) =>
  UserId ->
  FilePath ->
  DependentName ->
  ReleaseId ->
  PackageName ->
  Library ->
  ExceptT ImportError (DBT m) (PackageComponent, [Requirement])
extractFromLib userId directory dependentName releaseId packageName library = do
  let dependencies =
        filter (\d -> Cabal.depPkgName d /= "unbuildable") $
          library ^. (#libBuildInfo % #targetBuildDepends)
  let libraryName = getLibName $ library ^. #libName
  let componentType = Component.Library
  let canonicalForm = CanonicalComponent libraryName componentType
  component <- createComponent releaseId canonicalForm
  requirements <- mconcat <$> traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getLibName :: LibraryName -> Text
    getLibName LMainLibName = display packageName
    getLibName (LSubLibName lname) = T.pack $ unUnqualComponentName lname

-- extractFromExecutable :: UserId
--                       -> FilePath
--                       -> DependentName
--                       -> ReleaseId
--                       -> Executable
--                       -> ExceptT ImportError (DBT m) (PackageComponent, [Requirement])
-- extractFromExecutable userId directory dependentName releaseId executable = do
--   let dependencies = executable ^. #buildInfo ^. #targetBuildDepends
--   let executableName = getExecutableName $ executable ^. #exeName
--   let componentType = Component.Executable
--   let canonicalForm = CanonicalComponent executableName componentType
--   component <- createComponent releaseId canonicalForm
--   requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
--   pure (component, requirements)
--   where
--     getExecutableName :: UnqualComponentName -> Text
--     getExecutableName execName = T.pack $ unUnqualComponentName execName

-- extractFromTestSuite :: UserId
--                      -> FilePath
--                      -> DependentName
--                      -> ReleaseId -- ^
--                      -> TestSuite -- ^
--                      -> ExceptT ImportError (DBT m) (PackageComponent, [Requirement])
-- extractFromTestSuite userId directory dependentName releaseId testSuite = do
--   let dependencies = testSuite ^. #testBuildInfo ^. #targetBuildDepends
--   let testSuiteName = getTestSuiteName $ testSuite ^. #testName
--   let componentType = Component.TestSuite
--   let canonicalForm = CanonicalComponent testSuiteName componentType
--   component <- createComponent releaseId canonicalForm
--   requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
--   pure (component, requirements)
--   where
--     getTestSuiteName :: UnqualComponentName -> Text
--     getTestSuiteName testSuiteName = T.pack $ unUnqualComponentName testSuiteName

-- extractFromBenchmark :: UserId
--                      -> FilePath
--                      -> DependentName
--                      -> ReleaseId -- ^
--                      -> Benchmark -- ^
--                      -> ExceptT ImportError (DBT m) (PackageComponent, [Requirement])
-- extractFromBenchmark userId directory dependentName releaseId benchmark = do
--   let dependencies = benchmark ^. #benchmarkBuildInfo ^. #targetBuildDepends
--   let benchmarkName = getBenchmarkName $ benchmark ^. #benchmarkName
--   let componentType = Component.Benchmark
--   let canonicalForm = CanonicalComponent benchmarkName componentType
--   component <- createComponent releaseId canonicalForm
--   requirements <- traverse
--     (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
--   pure (component, requirements)
--   where
--     getBenchmarkName :: UnqualComponentName -> Text
--     getBenchmarkName benchName = T.pack $ unUnqualComponentName benchName

-- extractFromforeignLib :: UserId
--                       -> FilePath
--                       -> DependentName
--                       -> ReleaseId -- ^
--                       -> ForeignLib -- ^
--                       -> ExceptT ImportError (DBT m) (PackageComponent, [Requirement])
-- extractFromforeignLib userId directory dependentName releaseId foreignLib = do
--   let dependencies = foreignLib ^. #foreignLibBuildInfo ^. #targetBuildDepends
--   let foreignLibName = getforeignLibName $ foreignLib ^. #foreignLibName
--   let componentType = Component.ForeignLib
--   let canonicalForm = CanonicalComponent foreignLibName componentType
--   component <- createComponent releaseId canonicalForm
--   requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
--   pure (component, requirements)
--   where
--     getforeignLibName :: UnqualComponentName -> Text
--     getforeignLibName foreignLibName = T.pack $ unUnqualComponentName foreignLibName

depToRequirement ::
  (MonadIO m) =>
  -- | User associated with the import
  UserId ->
  -- | Directory of Cabal files
  FilePath ->
  -- | Namespace and PackageName of the package that requires it
  DependentName ->
  -- | Cabal datatype that expresses a dependency on a package name and a version
  Cabal.Dependency ->
  -- | The Id of the release's component that depends on this requirement
  ComponentId ->
  ExceptT ImportError (DBT m) [Requirement]
depToRequirement userId directory (dependentNamespace, dependentPackageName) cabalDependency packageComponentId = do
  let name = PackageName $ T.pack $ Cabal.unPackageName $ depPkgName cabalDependency
  let namespace = if Set.member name coreLibraries then Namespace "haskell" else Namespace "hackage"
  logImportMessage (dependentNamespace, dependentPackageName) $
    "Requiring @" <> display namespace <> "/" <> display name <> "…"
  logImportMessage (dependentNamespace, dependentPackageName) $
    "Creating Requirement for package component " <> display packageComponentId
      <> " on "
      <> "@"
      <> display namespace
      <> "/"
      <> display name
  result <- lift $ Query.getPackageByNamespaceAndName namespace name
  case result of
    Just package -> do
      logImportMessage (dependentNamespace, dependentPackageName) $
        "Required package: " <> "name: " <> display (package ^. #name)
      logImportMessage (dependentNamespace, dependentPackageName) $
        "Dependency @" <> display namespace <> "/" <> display name
          <> " is in the database"
      requirementId <- RequirementId <$> liftIO UUID.nextRandom
      let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
      let metadata = RequirementMetadata{flag = Nothing}
      let packageId = package ^. #packageId
      let req = Requirement{requirementId, packageId, packageComponentId, requirement, metadata}
      pure [req]
    Nothing
      | (dependentNamespace, dependentPackageName) == (namespace, name) -> do
          -- Checking if the package depends on itself
          -- Unused when loading only main components of packages
          logImportMessage (dependentNamespace, dependentPackageName) "A sub-component depends on the package itself."
          requirementId <- RequirementId <$> liftIO UUID.nextRandom
          let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
          let metadata = RequirementMetadata{flag = Nothing}
          let packageId = PackageId UUID.nil
          let req = Requirement{..}
          pure [req]
      | otherwise -> do
          logImportMessage (dependentNamespace, dependentPackageName) $
            "Dependency @" <> display namespace <> "/" <> display name
              <> " does not exist in the database, trying to import it from "
              <> T.pack directory
          let cabalPath = directory <> T.unpack (display name) <> ".cabal"
          package <- lift $ importCabal userId name cabalPath directory
          requirementId <- RequirementId <$> liftIO UUID.nextRandom
          let packageId = package ^. #packageId
          let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
          let metadata = RequirementMetadata{flag = Nothing}
          let req = Requirement{requirementId, packageComponentId, packageId, requirement, metadata}
          pure [req]

createComponent :: MonadIO m => ReleaseId -> CanonicalComponent -> ExceptT ImportError (DBT m) PackageComponent
createComponent releaseId canonicalForm = do
  componentId <- ComponentId <$> liftIO UUID.nextRandom
  pure PackageComponent{..}

createRelease :: (MonadIO m) => PackageId -> Version -> DBT m Release
createRelease packageId version = do
  releaseId <- ReleaseId <$> liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let archiveChecksum = mempty
  let createdAt = timestamp
  let updatedAt = timestamp
  pure Release{..}

cabalToPackage ::
  (MonadIO m) =>
  UserId ->
  PackageDescription ->
  Namespace ->
  PackageName ->
  ExceptT ImportError (DBT m) Package
cabalToPackage ownerId packageDesc namespace name = do
  timestamp <- liftIO getCurrentTime
  packageId <- PackageId <$> liftIO UUID.nextRandom
  sourceRepos <- getRepoURL (PackageName $ display $ packageDesc ^. (#package % #pkgName)) (packageDesc ^. #sourceRepos)
  let license = Cabal.license packageDesc
  let homepage = Just (display $ packageDesc ^. #homepage)
  let documentation = ""
  let bugTracker = Just (display $ packageDesc ^. #bugReports)
  let metadata = PackageMetadata{..}
  let synopsis = display $ packageDesc ^. #synopsis
  let createdAt = timestamp
  let updatedAt = timestamp
  pure $ Package{..}

-- getPackageName :: GenericPackageDescription -> ExceptT ImportError (DBT m) PackageName
-- getPackageName genDesc = do
--   let pkgName = display $ genDesc ^. #packageDescription ^. #package ^. #pkgName
--   case parsePackageName pkgName of
--     Nothing   -> throwError $ InvalidPackageName pkgName
--     Just name -> pure name

getRepoURL :: (MonadIO m) => PackageName -> [Cabal.SourceRepo] -> ExceptT ImportError (DBT m) [Text]
getRepoURL _ [] = pure []
getRepoURL _ (repo : _) = pure [display $ fromMaybe mempty (repo ^. #repoLocation)]

logImportMessage :: (MonadIO m) => (Namespace, PackageName) -> Text -> m ()
logImportMessage (namespace, name) message =
  liftIO $
    T.putStrLn $
      "[!] (@" <> display namespace <> "/" <> display name <> ")"
        <> " "
        <> message
        <> "\n"
