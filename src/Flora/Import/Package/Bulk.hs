{-# LANGUAGE AllowAmbiguousTypes #-}

module Flora.Import.Package.Bulk (importAllFilesInDirectory, importAllFilesInRelativeDirectory) where

import Control.Monad ((>=>), when)
import Data.List (isSuffixOf)
import Effectful
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB, getPool, runDB)
import Effectful.Time
import Log (Logger, defaultLogLevel)
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as SFold
import System.Directory qualified as System
import System.FilePath

import Flora.Import.Package (loadAndExtractCabalFile, persistImportOutput)
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory :: (DB :> es, IOE :> es) => Logger -> UserId -> FilePath -> Eff es ()
importAllFilesInRelativeDirectory appLogger user dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory appLogger user workdir

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: (DB :> es, IOE :> es) => Logger -> UserId -> FilePath -> Eff es ()
importAllFilesInDirectory appLogger user dir = do
  pool <- getPool
  liftIO $ System.createDirectoryIfMissing True dir
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> dir
  let displayCount =
        flip SFold.foldlM' (return 0) $
          \previousCount _ ->
            let currentCount = previousCount + 1
              in do
                when (currentCount `mod` 400 == 0) $
                  displayStats currentCount
                return currentCount
  processedPackageCount <- liftIO $ S.fold displayCount $ S.fromParallel $ S.mapM (processFile pool) $ findAllCabalFilesInDirectory dir
  displayStats processedPackageCount
  Update.refreshLatestVersions >> Update.refreshDependents
  where
    processFile pool =
        runEff
      . runDB pool
      . runCurrentTimeIO
      . Log.runLog "flora-jobs" appLogger defaultLogLevel
      . (loadAndExtractCabalFile user >=> persistImportOutput)
    displayStats :: MonadIO m => Int -> m ()
    displayStats currentCount =
      liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

findAllCabalFilesInDirectory :: FilePath -> S.ParallelT IO FilePath
findAllCabalFilesInDirectory workdir = S.concatMapM traversePath $ S.fromList [workdir]
  where
    traversePath p = do
      isDir <- liftIO $ System.doesDirectoryExist p
      case isDir of
        True -> do
          entries <- System.listDirectory p
          return $ S.concatMapM (traversePath . (p </>)) $ S.fromList entries
        False | ".cabal" `isSuffixOf` p -> return $ S.fromPure p
        _ -> return S.nil
