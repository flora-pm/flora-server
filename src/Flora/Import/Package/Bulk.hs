module Flora.Import.Package.Bulk (importAllFilesInDirectory, importAllFilesInRelativeDirectory) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import qualified System.Directory as System

import Control.Concurrent (getNumCapabilities, modifyMVar, newMVar)
import Control.Concurrent.Async (async, wait)
import Control.Monad
import Data.Function
import Database.PostgreSQL.Transact (DBT)
import Flora.Import.Package (loadAndExtractCabalFile, persistImportOutput)
import qualified Flora.Model.Package.Update as Update
import qualified Flora.Model.Release.Update as Update
import Flora.Model.User
import Streaming (chunksOf)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as Str
import System.FilePath

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory :: MonadIO m => UserId -> FilePath -> DBT m ()
importAllFilesInRelativeDirectory user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory user workdir

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: MonadIO m => UserId -> FilePath -> DBT m ()
importAllFilesInDirectory user dir = do
  parallelWorkers <- liftIO getNumCapabilities
  countMVar <- liftIO $ newMVar @Int 0
  findAllCabalFilesInDirectory dir
    & Str.mapM (liftIO . async . loadAndExtractCabalFile user)
    & chunksOf parallelWorkers
    & Str.mapped Str.toList
    & Str.mapM_ (persistChunk countMVar)
  Update.refreshLatestVersions
  Update.refreshDependents
  where
    persistChunk countMvar = traverse_ (processItem countMvar)
    processItem countMvar asyncImportOutput = do
      importOutput <- liftIO $ wait asyncImportOutput
      persistImportOutput importOutput
      newCount <- liftIO $ modifyMVar countMvar (\c -> pure (c + 1, c + 1))
      liftIO $ when (newCount `mod` 100 == 0) (putStrLn $ "✅ Processed " <> show newCount <> " new component releases")

{- | Finds all cabal files in the provided directory recursively
 Hits are written to the output channel as they are found, so it should be possible to process
 large amounts of Cabal files efficiently
-}
findAllCabalFilesInDirectory ::
  MonadIO m =>
  FilePath ->
  Stream (Of FilePath) m ()
findAllCabalFilesInDirectory workdir = do
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> workdir
  liftIO $ System.createDirectoryIfMissing True workdir
  inspectDir workdir
  where
    inspectDir dir = liftIO (System.listDirectory dir) >>= traverse_ (inspectItem dir)
    inspectItem dir item = do
      let fullPath = dir </> item
      isDir <- liftIO $ System.doesDirectoryExist fullPath
      case isDir of
        True -> inspectDir fullPath
        False | ".cabal" `isSuffixOf` fullPath -> Str.yield fullPath
        _ -> pure ()
