module Flora.Import.Package.Bulk (importAllFilesInDirectory, importAllFilesInRelativeDirectory) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import qualified System.Directory as System

import Control.Concurrent (getNumCapabilities, modifyMVar, newMVar)
import Control.Concurrent.Async (async, wait)
import Data.Function
import Data.Pool (Pool)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Connection)
import Flora.Import.Package (loadAndExtractCabalFile, persistImportOutput)
import qualified Flora.Model.Package.Update as Update
import qualified Flora.Model.Release.Update as Update
import Flora.Model.User
import Streaming (chunksOf)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as Str
import System.FilePath

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory :: MonadIO m => Pool Connection -> UserId -> FilePath -> m ()
importAllFilesInRelativeDirectory pool user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory pool user workdir

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: MonadIO m => Pool Connection -> UserId -> FilePath -> m ()
importAllFilesInDirectory pool user dir = do
  parallelWorkers <- liftIO getNumCapabilities
  let chunkSize = 200
  countMVar <- liftIO $ newMVar @Int 0
  findAllCabalFilesInDirectory dir
    & parMapM parallelWorkers (liftIO . loadAndExtractCabalFile user)
    & chunksOf chunkSize
    & Str.mapped Str.toList
    & Str.mapM_ (persistChunk countMVar)
  withPool pool $ Update.refreshLatestVersions >> Update.refreshDependents
  where
    persistChunk countMvar chunk = do
      let size = length chunk
      newCount <- liftIO $ modifyMVar countMvar (\c -> pure (c + size, c + size))
      withPool pool $ traverse_ persistImportOutput chunk
      liftIO . putStrLn $ "✅ Processed " <> show newCount <> " new cabal files"

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

-- | Replaces each element of a stream with the result of an action, processing elements of the stream concurrently
parMapM :: MonadIO m => Int -> (a -> IO b) -> Stream (Of a) m () -> Stream (Of b) m ()
parMapM concurrentActions f str =
  Str.mapM (liftIO . async . f) str
    & chunksOf concurrentActions
    & Str.mapped Str.toList
    & Str.mapM (liftIO . traverse wait)
    & Str.concat