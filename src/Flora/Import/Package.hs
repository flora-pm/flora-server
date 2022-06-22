{- |
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

import Control.Monad.Except
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Display
import qualified Data.Text.IO as T
import Data.Time
import Database.PostgreSQL.Transact
import Distribution.PackageDescription (allLibraries, unPackageName, unUnqualComponentName)
import qualified Distribution.PackageDescription as Cabal hiding (PackageName)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import qualified Distribution.Utils.ShortText as Cabal
import Optics.Core

import Data.Foldable
import Distribution.Verbosity (silent)
import qualified Flora.Import.Categories.Tuning as Tuning
import qualified Flora.Model.Category.Update as Update
import Flora.Model.Package.Component as Component
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types hiding (unPackageName)
import qualified Flora.Model.Package.Update as Update
import Flora.Model.Release
import qualified Flora.Model.Release.Update as Update
import Flora.Model.Requirement
  ( Requirement (..)
  , RequirementMetadata (..)
  , deterministicRequirementId
  , flag
  )
import Flora.Model.User
import GHC.Generics (Generic)
import qualified System.Directory as System
import System.FilePath

{- | This tuple represents the package that depends on any associated dependency/requirement.
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
    , PackageName "directory"
    , PackageName "deepseq"
    , PackageName "ghc-bignum"
    , PackageName "ghc-boot-th"
    , PackageName "ghc-prim"
    , PackageName "integer-simple"
    , PackageName "integer-gmp"
    , PackageName "mtl"
    , PackageName "parallel"
    , PackageName "parsec"
    , PackageName "process"
    , PackageName "rts"
    , PackageName "stm"
    , PackageName "text"
    , PackageName "transformers"
    , PackageName "unix"
    ]

{- | Imports a Cabal file into the database by:
   * first, reading and parsing the file using 'loadFile'
   * then, extracting relevant information using 'extractPackageDataFromCabal'
   * finally, inserting that data into the database
-}
importFile ::
  (MonadIO m) =>
  UserId ->
  -- | The absolute path to the Cabal file
  FilePath ->
  DBT m ()
importFile userId path = loadFile path >>= extractPackageDataFromCabal userId >>= persistImportOutput

importRelFile :: (MonadIO m) => UserId -> FilePath -> DBT m ()
importRelFile user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importFile user workdir

-- | Loads and parses a Cabal file
loadFile ::
  (MonadIO m) =>
  -- | The absolute path to the Cabal file
  FilePath ->
  m GenericPackageDescription
loadFile path = liftIO $ readGenericPackageDescription silent path

loadAndExtractCabalFile :: (MonadIO m) => UserId -> FilePath -> m ImportOutput
loadAndExtractCabalFile userId filePath = loadFile filePath >>= extractPackageDataFromCabal userId

{- | Persists an 'ImportOutput' to the database. An 'ImportOutput' can be obtained
 by extracting relevant information from a Cabal file using 'extractPackageDataFromCabal'
-}
persistImportOutput :: MonadIO m => ImportOutput -> DBT m ()
persistImportOutput (ImportOutput package categories release components) = do
  liftIO . T.putStrLn $ "📦  Persisting package: " <> packageName <> ", 🗓  Release v" <> display (release ^. #version)
  persistPackage
  Update.upsertRelease release
  traverse_ persistComponent components
  liftIO $ putStr "\n"
  where
    packageName = display (package ^. #namespace) <> "/" <> display (package ^. #name)
    persistPackage = do
      let packageId = package ^. #packageId
      Update.upsertPackage package
      forM_ categories (\case Tuning.NormalisedPackageCategory cat -> Update.addToCategoryByName packageId cat)

    persistComponent (packageComponent, deps) = do
      liftIO . T.putStrLn $
        "🧩  Persisting component: " <> display (packageComponent ^. #canonicalForm) <> " with " <> display (length deps) <> " 🔗 dependencies."
      Update.upsertPackageComponent packageComponent
      traverse_ persistImportDependency deps

    persistImportDependency dep = do
      Update.upsertPackage (dep ^. #package)
      Update.upsertRequirement (dep ^. #requirement)

{- | Transforms a 'GenericPackageDescription' from Cabal into an 'ImportOutput'
 that can later be inserted into the database. This function produces stable, deterministic ids,
 so it should be possible to extract and insert a single package many times in a row.
-}
extractPackageDataFromCabal :: MonadIO m => UserId -> GenericPackageDescription -> m ImportOutput
extractPackageDataFromCabal userId genericDesc = do
  let packageDesc = genericDesc ^. #packageDescription
  let packageName = packageDesc ^. #package % #pkgName % to unPackageName % to pack % to PackageName
  let packageVersion = packageDesc ^. #package % #pkgVersion
  let namespace = chooseNamespace packageName
  let packageId = deterministicPackageId namespace packageName
  let releaseId = deterministicReleaseId packageId packageVersion
  timestamp <- liftIO getCurrentTime
  sourceRepos <- getRepoURL packageName $ packageDesc ^. #sourceRepos
  let rawCategoryField = packageDesc ^. #category % to Cabal.fromShortText % to T.pack
  let categoryList = fmap (Tuning.UserPackageCategory . T.stripStart) (T.splitOn "," rawCategoryField)
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
          , homepage = packageDesc ^. #homepage % to display % to Just
          , documentation = ""
          , bugTracker = packageDesc ^. #bugReports % to display % to Just
          , maintainer = packageDesc ^. #maintainer % to display
          , synopsis = packageDesc ^. #synopsis % to display
          , description = packageDesc ^. #description % to display
          }

  let release =
        Release
          { releaseId
          , packageId
          , readme = Nothing
          , version = packageVersion
          , archiveChecksum = mempty
          , metadata = metadata
          , uploadedAt = Nothing
          , createdAt = timestamp
          , updatedAt = timestamp
          }
  libs <- traverse (extractLibrary package release) (allLibraries packageDesc)
  condLibs <- traverse (extractLibrary package release) (genericDesc ^.. #condLibrary % _Just % #condTreeData)
  -- TODO: import other components, currently we only import libraries
  let components = libs <> condLibs
  pure ImportOutput{..}

extractLibrary :: MonadIO m => Package -> Release -> Library -> m ImportComponent
extractLibrary package release lib = do
  let releaseId = release ^. #releaseId
  let componentName = lib ^. #libName % to getLibName
  let componentType = Component.Library
  let canonicalForm = CanonicalComponent{..}
  let componentId = deterministicComponentId releaseId canonicalForm
  let component = PackageComponent{..}
  let cabalDependencies = lib ^. #libBuildInfo % #targetBuildDepends % to filterDeps
  let dependencies = buildDependency componentId <$> cabalDependencies
  pure (component, dependencies)
  where
    filterDeps :: [Cabal.Dependency] -> [Cabal.Dependency]
    filterDeps = id

    getLibName :: LibraryName -> Text
    getLibName LMainLibName = display (package ^. #name)
    getLibName (LSubLibName lname) = T.pack $ unUnqualComponentName lname

    buildDependency :: ComponentId -> Cabal.Dependency -> ImportDependency
    buildDependency packageComponentId (Cabal.Dependency depName versionRange _) =
      let name = depName & unPackageName & pack & PackageName
          namespace = chooseNamespace name
          packageId = deterministicPackageId namespace name
          ownerId = package ^. #ownerId
          createdAt = package ^. #createdAt
          updatedAt = package ^. #updatedAt
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

getRepoURL :: (MonadIO m) => PackageName -> [Cabal.SourceRepo] -> m [Text]
getRepoURL _ [] = pure []
getRepoURL _ (repo : _) = pure [display $ fromMaybe mempty (repo ^. #repoLocation)]

chooseNamespace :: PackageName -> Namespace
chooseNamespace name | Set.member name coreLibraries = Namespace "haskell"
chooseNamespace _ = Namespace "hackage"
