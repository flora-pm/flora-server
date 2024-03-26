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
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import Effectful.Log qualified as Log
import Effectful.Poolboy
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Time (Time)
import Streamly.Data.Fold qualified as SFold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.Directory
import System.Directory qualified as System
import System.FilePath
import UnliftIO.Exception (finally)

import Effectful.Log (Log)
import Flora.Import.Package
  ( extractPackageDataFromCabal
  , loadContent
  , persistImportOutput
  )
import Flora.Model.Package
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User
import GHC.Conc (numCapabilities)
import Streamly.Data.Stream.Prelude (maxThreads, ordered)

-- | Same as 'importAllFilesInDirectory' but accepts a relative path to the current working directory
importAllFilesInRelativeDirectory
  :: (Log :> es, Time :> es, FileSystem :> es, Poolboy :> es, DB :> es, IOE :> es)
  => UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInRelativeDirectory user (repositoryName, repositoryURL) dir = do
  workdir <- (</> dir) <$> liftIO System.getCurrentDirectory
  importAllFilesInDirectory user (repositoryName, repositoryURL) workdir

importFromIndex
  :: (Time :> es, Log :> es, Poolboy :> es, DB :> es, IOE :> es)
  => UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importFromIndex user (repositoryName, repositoryURL) index = do
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
  case Tar.foldlEntries (buildContentStream time) Streamly.nil entries of
    Right stream ->
      importFromStream
        user
        (repositoryName, repositoryURL, repositoryPackages)
        stream
    Left (err, _) ->
      Log.logAttention_ $
        "Failed to get files from index: " <> Text.pack (show err)
  where
    buildContentStream
      :: UTCTime
      -> Stream (Eff es) (FilePath, UTCTime, StrictByteString)
      -> Tar.GenEntry Tar.TarPath linkTarget
      -> Stream (Eff es) (FilePath, UTCTime, StrictByteString)
    buildContentStream time acc entry =
      let entryPath = Tar.entryPath entry
          entryTime = posixSecondsToUTCTime . fromIntegral $ Tar.entryTime entry
       in Tar.entryContent entry & \case
            Tar.NormalFile bs _
              | ".cabal" `isSuffixOf` entryPath && entryTime > time ->
                  (entryPath, entryTime, BL.toStrict bs) `Streamly.cons` acc
            _ -> acc

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory
  :: (Time :> es, Log :> es, FileSystem :> es, Poolboy :> es, DB :> es, IOE :> es)
  => UserId
  -> (Text, Text)
  -> FilePath
  -> Eff es ()
importAllFilesInDirectory user (repositoryName, repositoryURL) dir = do
  liftIO $ System.createDirectoryIfMissing True dir
  packages <- buildPackageListFromDirectory dir
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> dir
  importFromStream user (repositoryName, repositoryURL, packages) (findAllCabalFilesInDirectory dir)

importFromStream
  :: forall es
   . (Time :> es, Log :> es, Poolboy :> es, DB :> es, IOE :> es)
  => UserId
  -> (Text, Text, Set PackageName)
  -> Stream (Eff es) (String, UTCTime, StrictByteString)
  -> Eff es ()
importFromStream user (repositoryName, _repositoryURL, repositoryPackages) stream = do
  let cfg = maxThreads numCapabilities . ordered True
  processedPackageCount <-
    finally
      ( Streamly.fold displayCount $
          Streamly.parMapM cfg processFile stream
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
    displayCount :: SFold.Fold (Eff es) a Int
    displayCount =
      flip SFold.foldlM' (pure 0) $
        \previousCount _ ->
          let currentCount = previousCount + 1
           in do
                when (currentCount `mod` 400 == 0) $
                  displayStats currentCount
                pure currentCount
    processFile :: (String, UTCTime, StrictByteString) -> Eff es ()
    processFile (path, timestamp, content) =
      loadContent path content
        >>= ( extractPackageDataFromCabal user (repositoryName, repositoryPackages) timestamp
                >=> \importedPackage -> persistImportOutput importedPackage
            )
    displayStats :: Int -> Eff es ()
    displayStats currentCount =
      liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

findAllCabalFilesInDirectory
  :: forall es
   . FileSystem :> es
  => FilePath
  -> Stream (Eff es) (String, UTCTime, StrictByteString)
findAllCabalFilesInDirectory workdir = Streamly.concatMapM traversePath $ Streamly.fromList [workdir]
  where
    traversePath :: FilePath -> Eff es (Stream (Eff es) (FilePath, UTCTime, StrictByteString))
    traversePath p = do
      isDir <- FileSystem.doesDirectoryExist p
      case isDir of
        True -> do
          entries <- FileSystem.listDirectory p
          pure $ Streamly.concatMapM (traversePath . (p </>)) $ Streamly.fromList entries
        False | ".cabal" `isSuffixOf` p -> do
          content <- FileSystem.readFile p
          timestamp <- FileSystem.getModificationTime p
          pure $ Streamly.fromPure (p, timestamp, content)
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
