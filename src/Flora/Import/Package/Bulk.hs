{-# LANGUAGE AllowAmbiguousTypes #-}

module Flora.Import.Package.Bulk (importAllFilesInDirectory, importAllFilesInRelativeDirectory) where

import Control.Concurrent (getNumCapabilities, modifyMVar, newMVar)
import Control.Concurrent.Async (async, wait)
import Data.Foldable (traverse_)
import Data.Function
import Data.List (isSuffixOf)
import Effectful
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB, getPool, runDB)
import Effectful.Time
import Log (Logger, defaultLogLevel)
import Streaming (chunksOf)
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as Str
import System.Directory qualified as System
import System.FilePath

import Flora.Import.Package (loadAndExtractCabalFile, persistImportOutput)
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory :: [DB, IOE] :>> es => Logger -> UserId -> FilePath -> Eff es ()
importAllFilesInRelativeDirectory appLogger user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory appLogger user workdir

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: ([DB, IOE] :>> es) => Logger -> UserId -> FilePath -> Eff es ()
importAllFilesInDirectory appLogger user dir = do
  pool <- getPool
  parallelWorkers <- liftIO getNumCapabilities
  let chunkSize = 200
  countMVar <- liftIO $ newMVar @Int 0
  findAllCabalFilesInDirectory dir
    & parMapM parallelWorkers (runEff . runDB pool . runCurrentTimeIO . Log.runLogging "flora-jobs" appLogger defaultLogLevel . loadAndExtractCabalFile user)
    & chunksOf chunkSize
    & Str.mapped Str.toList
    & Str.mapM_ (persistChunk countMVar)
  Update.refreshLatestVersions >> Update.refreshDependents
  where
    persistChunk countMvar chunk = do
      let size = length chunk
      newCount <- liftIO $ modifyMVar countMvar (\c -> pure (c + size, c + size))
      traverse_ persistImportOutput chunk
      liftIO . putStrLn $ "✅ Processed " <> show newCount <> " new cabal files"

{- | Finds all cabal files in the provided directory recursively
 Hits are written to the output channel as they are found, so it should be possible to process
 large amounts of Cabal files efficiently
-}
findAllCabalFilesInDirectory ::
  IOE :> es =>
  FilePath ->
  Stream (Of FilePath) (Eff es) ()
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
parMapM :: IOE :> es => Int -> (a -> IO b) -> Stream (Of a) (Eff es) () -> Stream (Of b) (Eff es) ()
parMapM concurrentActions f str =
  Str.mapM (liftIO . async . f) str
    & chunksOf concurrentActions
    & Str.mapped Str.toList
    & Str.mapM (liftIO . traverse wait)
    & Str.concat
