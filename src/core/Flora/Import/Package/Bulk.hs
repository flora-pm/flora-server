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
import Data.Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import GHC.Conc (numCapabilities)
import GHC.Records
import Streamly.Data.Fold qualified as SFold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude (maxThreads, ordered)
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.Directory
import System.Directory qualified as System
import System.FilePath
import UnliftIO.Exception (finally)

import Effectful.Poolboy
import Flora.Environment.Env
import Flora.Import.Package
  ( extractPackageDataFromCabal
  , loadContent
  , persistImportOutput
  )
import Flora.Import.Types (ImportFileType (..))
import Flora.Model.Package
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User
import Flora.Monitoring

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory
  :: ( DB :> es
     , FileSystem :> es
     , HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , IOE :> es
     , Log :> es
     , Poolboy :> es
     , Reader r :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInRelativeDirectory user (repositoryName, repositoryURL) dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory user (repositoryName, repositoryURL) workdir

importFromIndex
  :: ( DB :> es
     , HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , IOE :> es
     , Log :> es
     , Poolboy :> es
     , Reader r :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => UserId
  -> Text
  -> FilePath
  -> Eff es ()
importFromIndex user repositoryName index = do
  entries <- Tar.read . GZip.decompress <$> liftIO (BL.readFile index)
  let Right repositoryPackages = buildPackageListFromArchive entries
  Log.logInfo "packages" $
    object
      [ "repository" .= repositoryName
      , "packages" .= repositoryPackages
      ]
  mPackageIndex <- Query.getPackageIndexByName repositoryName
  time <- case mPackageIndex of
    Nothing -> pure $ posixSecondsToUTCTime 0
    Just packageIndex ->
      pure $
        fromMaybe
          (posixSecondsToUTCTime 0)
          packageIndex.timestamp
  case Tar.foldlEntries (buildContentStream time) Streamly.nil entries of
    Right stream ->
      importFromStream
        user
        (repositoryName, repositoryPackages)
        stream
    Left (err, _) ->
      Log.logAttention_ $
        "Failed to get files from index: " <> Text.pack (show err)
  where
    buildContentStream
      :: UTCTime
      -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
      -> Tar.GenEntry Tar.TarPath linkTarget
      -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
    buildContentStream time acc entry =
      let entryPath = Tar.entryPath entry
          entryTime = posixSecondsToUTCTime . fromIntegral $ Tar.entryTime entry
       in Tar.entryContent entry & \case
            Tar.NormalFile bs _
              | ".cabal" `isSuffixOf` entryPath && entryTime > time ->
                  (CabalFile entryPath, entryTime, BL.toStrict bs) `Streamly.cons` acc
            -- \| ".json" `isSuffixOf` entryPath && entryTime > time ->
            --     (JSONFile entryPath, entryTime, BL.toStrict bs) `Streamly.cons` acc
            _ -> acc

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory
  :: ( DB :> es
     , FileSystem :> es
     , HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , IOE :> es
     , Log :> es
     , Poolboy :> es
     , Reader r :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInDirectory user (repositoryName, _repositoryURL) dir = do
  liftIO $ System.createDirectoryIfMissing True dir
  packages <- buildPackageListFromDirectory dir
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> dir
  importFromStream user (repositoryName, packages) (findAllCabalFilesInDirectory dir)

importFromStream
  :: forall es r
   . ( DB :> es
     , HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , IOE :> es
     , Log :> es
     , Poolboy :> es
     , Reader r :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => UserId
  -> (Text, Set PackageName)
  -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
  -> Eff es ()
importFromStream userId repository@(repositoryName, _) stream = do
  let cfg = maxThreads numCapabilities . ordered True
  processedPackageCount <-
    finally
      ( Streamly.fold displayCount $
          Streamly.parMapM cfg (processFile userId repository) stream
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
  increasePackageImportCounterBy
    (fromIntegral @Int @Double processedPackageCount)
    repositoryName
  where
    displayCount :: SFold.Fold (Eff es) a Int
    displayCount =
      flip SFold.foldlM' (pure 0) $
        \previousCount _ -> do
          let currentCount = previousCount + 1
              batchAmount = 400
          when (currentCount `mod` batchAmount == 0) $ displayStats currentCount
          pure currentCount

displayStats
  :: IOE :> es
  => Int
  -> Eff es ()
displayStats currentCount = do
  liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

processFile
  :: ( DB :> es
     , IOE :> es
     , Log :> es
     , Poolboy :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     )
  => UserId
  -> (Text, Set PackageName)
  -> (ImportFileType, UTCTime, StrictByteString)
  -> Eff es ()
processFile userId repository importSubject =
  case importSubject of
    (CabalFile path, timestamp, content) -> do
      Log.logInfo "importing-package" $
        object ["file_path" .= path]
      loadContent path content
        >>= ( extractPackageDataFromCabal userId repository timestamp
                >=> \importedPackage -> persistImportOutput importedPackage
            )

-- (JSONFile path, _, content) ->
--   do
--     loadJSONContent path content repository
--     >>= persistHashes

findAllCabalFilesInDirectory
  :: forall es
   . FileSystem :> es
  => FilePath
  -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
findAllCabalFilesInDirectory workdir = Streamly.concatMapM traversePath $ Streamly.fromList [workdir]
  where
    traversePath :: FilePath -> Eff es (Stream (Eff es) (ImportFileType, UTCTime, StrictByteString))
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
