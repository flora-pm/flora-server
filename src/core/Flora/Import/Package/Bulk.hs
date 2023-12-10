{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Flora.Import.Package.Bulk
  ( importAllFilesInDirectory
  , importAllFilesInRelativeDirectory
  , importFromIndex
  ) where

import Codec.Archive.Tar (Entries)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Tar.Index qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (when, (>=>))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Poolboy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB, getPool, runDB)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Time (Time, runTime)
import Log (Logger, defaultLogLevel)
import Streamly.Data.Fold qualified as SFold
import Streamly.Prelude qualified as S
import System.Directory
import System.Directory qualified as System
import System.FilePath
import UnliftIO.Exception (finally)

import Flora.Environment.Config (PoolConfig (..))
import Flora.Import.Package
  ( extractPackageDataFromCabal
  , loadContent
  , loadJSONContent
  , persistHashes
  , persistImportOutput
  , withWorkerDbPool
  )
import Flora.Import.Types
import Flora.Model.Package
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory
  :: (Reader PoolConfig :> es, DB :> es, IOE :> es)
  => Logger
  -> UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInRelativeDirectory appLogger user (repositoryName, repositoryURL) dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory appLogger user (repositoryName, repositoryURL) workdir

importFromIndex
  :: (Reader PoolConfig :> es, DB :> es, IOE :> es)
  => Logger
  -> UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importFromIndex appLogger user (repositoryName, repositoryURL) index = do
  entries <- Tar.read . GZip.decompress <$> liftIO (BL.readFile index)
  let Right repositoryPackages = buildPackageListFromArchive entries
  mPackageIndex <- Query.getPackageIndexByName repositoryName
  time <- case mPackageIndex of
    Nothing -> pure $ posixSecondsToUTCTime 0
    Just packageIndex ->
      pure $
        fromMaybe
          (posixSecondsToUTCTime 0)
          packageIndex.timestamp
  case Tar.foldlEntries (buildContentStream time) S.nil entries of
    Right stream ->
      importFromStream
        appLogger
        user
        (repositoryName, repositoryURL, repositoryPackages)
        stream
    Left (err, _) ->
      Log.runLog "flora-cli" appLogger defaultLogLevel $
        Log.logAttention_ $
          "Failed to get files from index: " <> Text.pack (show err)
  where
    buildContentStream time acc entry =
      let entryPath = Tar.entryPath entry
          entryTime = posixSecondsToUTCTime . fromIntegral $ Tar.entryTime entry
       in Tar.entryContent entry & \case
            Tar.NormalFile bs _
              | ".cabal" `isSuffixOf` entryPath && entryTime > time ->
                  (CabalFile entryPath, entryTime, BL.toStrict bs) `S.cons` acc
              | ".json" `isSuffixOf` entryPath && entryTime > time ->
                  (JSONFile entryPath, entryTime, BL.toStrict bs) `S.cons` acc
            _ -> acc

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory
  :: (Reader PoolConfig :> es, DB :> es, IOE :> es)
  => Logger
  -> UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInDirectory appLogger user (repositoryName, repositoryURL) dir = do
  liftIO $ System.createDirectoryIfMissing True dir
  packages <- buildPackageListFromDirectory dir
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> dir
  importFromStream appLogger user (repositoryName, repositoryURL, packages) $ findAllCabalFilesInDirectory dir

importFromStream
  :: (Reader PoolConfig :> es, DB :> es, IOE :> es)
  => Logger
  -> UserId
  -> (Text, Text, Set PackageName)
  -> S.AsyncT IO (ImportFileType, UTCTime, BS.ByteString)
  -> Eff es ()
importFromStream appLogger user (repositoryName, repositoryURL, repositoryPackages) stream = do
  pool <- getPool
  poolConfig <- ask @PoolConfig
  processedPackageCount <-
    finally
      ( withWorkerDbPool $ \wq ->
          liftIO $
            S.fold displayCount $
              S.fromAsync $
                S.mapM
                  ( \streamItem ->
                      runProcessFile pool poolConfig wq (processFile streamItem)
                  )
                  stream
      )
      -- We want to refresh db and update latest timestamp even if we fell
      -- over at some point
      ( do
          Update.refreshLatestVersions
          Update.refreshDependents
          timestamp <- Query.getLatestReleaseTime (Just repositoryName)
          Update.updatePackageIndexByName repositoryName timestamp
      )
  displayStats processedPackageCount
  where
    displayCount =
      flip SFold.foldlM' (return 0) $
        \previousCount _ ->
          let currentCount = previousCount + 1
           in do
                when (currentCount `mod` 400 == 0) $
                  displayStats currentCount
                pure currentCount

    runProcessFile
      :: Pool Connection
      -> poolConfig
      -> WorkQueue
      -> (WorkQueue -> Eff '[Reader poolConfig, DB, Time, Log, IOE] ())
      -> IO ()
    runProcessFile pool poolConfig wq action =
      action wq
        & runReader poolConfig
        & runDB pool
        & runTime
        & Log.runLog "flora-cli" appLogger defaultLogLevel
        & runEff

    processFile
      :: (Log :> es, IOE :> es, Time :> es, DB :> es)
      => (ImportFileType, UTCTime, BS.ByteString)
      -> WorkQueue
      -> Eff es ()
    processFile importSubject wq =
      case importSubject of
        (CabalFile path, timestamp, content) ->
          loadContent path content
            >>= ( extractPackageDataFromCabal user (repositoryName, repositoryPackages) timestamp
                    >=> \importedPackage -> persistImportOutput wq importedPackage
                )
        (JSONFile path, _, content) ->
          do
            loadJSONContent path content (repositoryName, repositoryPackages)
            >>= persistHashes

    displayStats :: MonadIO m => Int -> m ()
    displayStats currentCount =
      liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

findAllCabalFilesInDirectory :: FilePath -> S.AsyncT IO (ImportFileType, UTCTime, BS.ByteString)
findAllCabalFilesInDirectory workdir = S.concatMapM traversePath $ S.fromList [workdir]
  where
    traversePath p = do
      isDir <- liftIO $ System.doesDirectoryExist p
      case isDir of
        True -> do
          entries <- System.listDirectory p
          return $ S.concatMapM (traversePath . (p </>)) $ S.fromList entries
        False | ".cabal" `isSuffixOf` p -> do
          content <- BS.readFile p
          timestamp <- System.getModificationTime p
          return $ S.fromPure (CabalFile p, timestamp, content)
        _ -> return S.nil

buildPackageListFromArchive :: Entries e -> Either e (Set PackageName)
buildPackageListFromArchive entries =
  case Tar.build entries of
    Left e -> Left e
    Right tarIndex ->
      Tar.toList tarIndex
        & fmap (takeDirectory . takeDirectory . fst)
        & filter (/= ".")
        & fmap (PackageName . Text.pack)
        & Set.fromList
        & Right

buildPackageListFromDirectory :: IOE :> es => FilePath -> Eff es (Set PackageName)
buildPackageListFromDirectory dir = do
  paths <- liftIO $ listDirectory dir
  paths
    & fmap takeBaseName
    & filter (/= ".")
    & fmap (PackageName . Text.pack)
    & Set.fromList
    & pure
