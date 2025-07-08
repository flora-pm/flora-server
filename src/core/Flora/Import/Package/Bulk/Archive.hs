{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Flora.Import.Package.Bulk.Archive
  ( importFromArchive
  , buildPackageListFromArchive
  ) where

import Codec.Archive.Tar (Entries)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Tar.Index qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad
import Data.Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Optics.Core
import RequireCallStack
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.FilePath

import Flora.Environment.Env
import Flora.Import.Package.Bulk.Stream
import Flora.Import.Types (ImportError, ImportFileType (..))
import Flora.Model.Package hiding (PackageName)
import Flora.Model.Package qualified as Flora
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Monad

importFromArchive
  :: ( Concurrent :> es
     , DB :> es
     , Error ImportError :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => Text
  -> Vector Text
  -> FilePath
  -> FloraM es ()
importFromArchive repositoryName indexDependencies indexArchiveBasePath = do
  let indexArchivePath = indexArchiveBasePath <> "/" <> (Text.unpack repositoryName) <> "/01-index.tar.gz"
  entries <- Tar.read . GZip.decompress <$> liftIO (BL.readFile indexArchivePath)
  indexPackages <- do
    let Right localPackages = buildPackageListFromArchive entries
    when (null localPackages) $ error $ "Index " <> Text.unpack repositoryName <> " has no entries!"
    dependencyPackages <- forM indexDependencies $ \dep -> do
      let depArchivePath = indexArchiveBasePath <> "/" <> (Text.unpack dep) <> "/01-index.tar.gz"
      indexDependencyEntries <- Tar.read . GZip.decompress <$> liftIO (BL.readFile depArchivePath)
      let Right indexPackages = buildPackageListFromArchive indexDependencyEntries
      when (null indexPackages) $ error $ "Index dependency " <> Text.unpack dep <> " has no entries!"
      pure (dep, indexPackages)
    pure $ (repositoryName, localPackages) `Vector.cons` dependencyPackages

  Log.logInfo "importing packages" $
    object
      [ "repository" .= repositoryName
      , "packages" .= indexPackages
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
        repositoryName
        indexPackages
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

-- (JSONFile path, _, content) ->
--   do
--     loadJSONContent path content repository
--     >>= persistHashes

buildPackageListFromArchive :: Entries e -> Either e (Set Flora.PackageName)
buildPackageListFromArchive entries =
  case Tar.build entries of
    Left e -> Left e
    Right tarIndex ->
      Tar.toList tarIndex
        & filter (\(filename, _) -> ".cabal" `isSuffixOf` filename)
        & fmap (takeDirectory . takeDirectory . fst)
        & fmap (Flora.PackageName . Text.pack)
        & Set.fromList
        & Right
