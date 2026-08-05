{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Flora.Domain.Import.Package.Bulk.Archive
  ( importFromArchive
  , buildPackageListFromArchive

    -- * Exposed for testing
  , scanIndex
  ) where

import Codec.Archive.Tar (Entries)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Effectful.Tracing (Tracer)
import RequireCallStack
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import System.FilePath

import Flora.Domain.Import.Package.Bulk.Stream
import Flora.Domain.Import.Types (ImportError (..), ImportFileType (..), ImportSubject)
import Flora.Environment.Env
import Flora.Model.Package.Types qualified as Flora
import Flora.Model.PackageIndex.Guard
import Flora.Model.PackageIndex.Types
import Flora.Monad

-- | The index archive is traversed twice:
-- 1. To collect the package names and count the cabal revisions we are going to import;
-- 2. To stream those cabal files out for processing.
importFromArchive
  :: ( Concurrent :> es
     , Error ImportError :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , Time :> es
     , Tracer :> es
     )
  => Pool PG.Connection
  -> Text
  -> Vector Text
  -> FilePath
  -> FloraM es ()
importFromArchive pool repositoryName indexDependencies indexArchiveBasePath = do
  packageIndex <- guardThatPackageIndexExists pool repositoryName $ do
    Log.logAttention "Could not find package index" $
      object
        [ "package_index_name" .= repositoryName
        ]
    Error.throwError (CouldNotFindPackageIndex repositoryName)
  let time = fromMaybe (posixSecondsToUTCTime 0) packageIndex.timestamp

  localArchive <- readArchive (archivePathFor repositoryName)

  (localPackages, revisionCounts) <-
    scanOrThrow repositoryName fst (scanIndex time (entriesOf localArchive))

  dependencyPackages <- forM indexDependencies $ \dep -> do
    entries <- entriesOf <$> readArchive (archivePathFor dep)
    depPackages <- scanOrThrow dep id (buildPackageListFromArchive entries)
    pure (dep, depPackages)
  let indexPackages = (repositoryName, localPackages) `Vector.cons` dependencyPackages

  importFromStream
    pool
    packageIndex
    indexPackages
    (contentStream packageIndex time revisionCounts (entriesOf localArchive))
  where
    archivePathFor name =
      indexArchiveBasePath <> "/" <> Text.unpack name <> "/01-index.tar.gz"

    readArchive path = BL.fromStrict <$> liftIO (BS.readFile path)

    entriesOf = Tar.read . GZip.decompress

-- | Stream the cabal files to import, one per package version, lazily.
--
-- NOTE: Only the last entry for a given path is yielded,
-- in order to get the latest metadata revision.
contentStream
  :: (Log :> es, Show e)
  => PackageIndex
  -> UTCTime
  -> Map Text Int
  -> Entries e
  -> Stream (Eff es) ImportSubject
contentStream packageIndex time revisionCounts contentEntries =
  -- `Stream.unfoldrM` is better than `Stream.cons` because the latter
  -- gives you O(n²) time complexity.
  --
  -- Paths with a single entry are dropped from the countdown map: 'step' reads
  -- an absent key as "one entry left", so keeping them would cost one map entry
  -- per package version of the index for no change in behaviour. Forced here so
  -- that an import with nothing to do does not retain the unfiltered map.
  Stream.unfoldrM (uncurry step) (contentEntries, dups)
  where
    !dups = Map.filter (> 1) revisionCounts

    subject entry path entryTime contents =
      ( CabalFile path
      , entryTime
      , uploaderName packageIndex entry
      , BL.toStrict contents
      )

    step Tar.Done _ = pure Nothing
    step (Tar.Fail err) _ = do
      Log.logAttention
        "Stopping the import early, could not read the rest of the index: "
        $ object ["error" .= Text.pack (show err)]
      pure Nothing
    step (Tar.Next entry rest) counts
      | Just (path, entryTime, contents) <- cabalEntry entry
      , isFresh time entryTime =
          let key = Text.pack path
              remaining = Map.findWithDefault 1 key counts - 1
           in if remaining > 0
                then step rest (Map.insert key remaining counts)
                else pure $ Just (subject entry path entryTime contents, (rest, Map.delete key counts))
      | otherwise = step rest counts

-- | Fold over the cabal entries of an index archive, ignoring everything else.
--
-- NOTE: Partial results of a truncated archive are discarded.
foldCabalEntries :: (a -> (FilePath, UTCTime) -> a) -> a -> Entries e -> Either e a
foldCabalEntries f z entries =
  first fst $ Tar.foldlEntries step z entries
  where
    step acc entry =
      case cabalEntry entry of
        Nothing -> acc
        Just (path, entryTime, _) -> f acc (path, entryTime)

-- | One traversal collecting
-- 1. Which packages the index declares (the `names` acc), for correctness
--    of namespace selection.
-- 2. How many revisions each cabal file has (the `count` acc).
scanIndex :: UTCTime -> Entries e -> Either e (Set Flora.PackageName, Map Text Int)
scanIndex time = foldCabalEntries step (Set.empty, Map.empty)
  where
    step (names, counts) (path, entryTime) =
      let !names' = Set.insert (packageNameFromPath path) names
          !counts'
            | isFresh time entryTime = Map.insertWith (+) (Text.pack path) 1 counts
            | otherwise = counts
       in (names', counts')

-- | The package names declared by an index archive.
--
-- Unlike 'scanIndex' this counts no revisions: an index we merely resolve
-- namespaces against is never imported from, so there is nothing to count.
buildPackageListFromArchive :: Entries e -> Either e (Set Flora.PackageName)
buildPackageListFromArchive = foldCabalEntries step Set.empty
  where
    step names (path, _) = Set.insert (packageNameFromPath path) names

-- | Is this entry a cabal file at all?
--
-- NOTE: This could be the place where we distinguish
-- between cabal and json entries.
cabalEntry :: Tar.GenEntry LazyByteString Tar.TarPath linkTarget -> Maybe (FilePath, UTCTime, LazyByteString)
cabalEntry entry =
  case Tar.entryContent entry of
    Tar.NormalFile contents _
      | ".cabal" `isSuffixOf` path ->
          Just (path, entryTime, contents)
    _ -> Nothing
  where
    path = Tar.entryPath entry
    entryTime = posixSecondsToUTCTime . fromIntegral $ Tar.entryTime entry

-- | Is this entry newer than the last timestamp we recorded for its index?
--
-- Both traversals of an archive have to agree on this, or the revision
-- countdown of 'contentStream' counts entries the stream never sees.
isFresh :: UTCTime -> UTCTime -> Bool
isFresh time entryTime = entryTime > time

-- | Index paths are @\<package\>/\<version\>/\<package\>.cabal@.
packageNameFromPath :: FilePath -> Flora.PackageName
packageNameFromPath path =
  path
    & takeDirectory
    & takeDirectory
    & Text.pack
    & Flora.PackageName

uploaderName :: PackageIndex -> Tar.GenEntry tarPath linkTarget content -> Maybe Text
uploaderName packageIndex entry
  | packageIndex.repository /= "hackage" = Nothing
  | null entry.entryOwnership.ownerName = Nothing
  | otherwise = Just (Text.pack entry.entryOwnership.ownerName)

-- | Refuse an index we could not read, or one that declares no package at all.
scanOrThrow
  :: (Error ImportError :> es, Log :> es, Show e)
  => Text
  -> (a -> Set Flora.PackageName)
  -> Either e a
  -> FloraM es a
scanOrThrow indexName declaredNames result = do
  scanned <- case result of
    Left err -> do
      Log.logAttention "Could not parse package index" $
        object ["package_index_name" .= indexName, "error" .= show err]
      Error.throwError $ MalformedPackageIndex indexName (Text.pack (show err))
    Right ok -> pure ok
  when (null (declaredNames scanned)) $ do
    Log.logAttention "Package index has no entries" $ object ["package_index_name" .= indexName]
    Error.throwError $ EmptyPackageIndex indexName
  pure scanned
