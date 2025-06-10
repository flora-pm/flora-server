module Flora.Import.Package.Bulk.Directory
  ( importAllFilesInDirectory
  , buildPackageListFromDirectory
  ) where

import Control.DeepSeq (force)
import Control.Monad (forM)
import Data.ByteString (StrictByteString)
import Data.List (isSuffixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName as Cabal
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Optics.Core
import RequireCallStack
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.Directory qualified as System
import System.FilePath

import Flora.Environment.Env
import Flora.Import.Package
  ( loadContent
  )
import Flora.Import.Package.Bulk.Stream
import Flora.Import.Types (ImportFileType (..))
import Flora.Model.Package hiding (PackageName)
import Flora.Model.Package qualified as Flora
import Flora.Monad

-- | Finds all cabal files in the specified baseDir ectory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory
  :: ( Concurrent :> es
     , DB :> es
     , FileSystem :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => Text
  -> FilePath
  -> Vector Text
  -> FilePath
  -> FloraM es ()
importAllFilesInDirectory repositoryName repositoryDir indexDependencies baseDir = do
  traceM $ "Base directory: " <> baseDir
  liftIO $ System.createDirectoryIfMissing True baseDir
  packages <- do
    let localPackagesDir = baseDir </> repositoryDir
    traceM $ "Local packages directory: " <> localPackagesDir
    localPackages <- buildPackageListFromDirectory localPackagesDir
    dependencyPackages <- forM indexDependencies $ \dep -> do
      let indexDependenciesPath = baseDir <> "/" <> Text.unpack dep
      traceM $ "Index dependency directory: " <> indexDependenciesPath
      packages <- buildPackageListFromDirectory indexDependenciesPath
      pure (dep, packages)
    pure $ (repositoryName, localPackages) `Vector.cons` dependencyPackages
  liftIO . putStrLn $ "🔎  Searching cabal files in: " <> baseDir
  importFromStream repositoryName packages (findAllCabalFilesInDirectory baseDir)

findAllCabalFilesInDirectory
  :: forall es
   . FileSystem :> es
  => FilePath
  -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
findAllCabalFilesInDirectory workdir = Streamly.concatMapM traversePath $ Streamly.fromList [workdir]
  where
    traversePath :: FileSystem :> es1 => FilePath -> Eff es1 (Stream (Eff es1) (ImportFileType, UTCTime, StrictByteString))
    traversePath p = do
      isDir <- FileSystem.doesDirectoryExist p
      case isDir of
        True -> do
          entries <- FileSystem.listDirectory p
          pure $ Streamly.concatMapM (traversePath . (p </>)) $ Streamly.fromList entries
        False | ".cabal" `isSuffixOf` p -> do
          content <- FileSystem.readFile p
          timestamp <- FileSystem.getModificationTime p
          pure $ Streamly.fromPure (CabalFile p, timestamp, content)
        _ -> pure Streamly.nil

buildPackageListFromDirectory
  :: (FileSystem :> es, Log :> es, RequireCallStack)
  => FilePath
  -> FloraM es (Set Flora.PackageName)
buildPackageListFromDirectory dir = do
  traceM $ "Reading directory: " <> dir
  paths <- FileSystem.listDirectory dir
  result <- forM paths $ \path -> do
    content <- FileSystem.readFile (dir </> path)
    genericDesc <- loadContent (dir </> path) content
    let packageDesc = genericDesc.packageDescription
    let packageName = force $ packageDesc ^. #package % #pkgName % to unPackageName % to Text.pack % to Flora.PackageName
    pure packageName
  pure $ Set.fromList result
