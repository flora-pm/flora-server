{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Helpers for deriving advisory metadata from a Snapshot s.
module Security.Advisories.Sync.Snapshot
  ( SnapshotDirectoryInfo (..),
    ETag (..),
    SnapshotError (..),
    explainSnapshotError,
    SnapshotUrl (..),
    SnapshotRepositoryEnsuredStatus (..),
    ensureSnapshot,
    getDirectorySnapshotInfo,
    overwriteSnapshot,
    ensureEmptyRoot,
    SnapshotRepositoryStatus (..),
    snapshotRepositoryStatus,
    latestUpdate,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (Exception (displayException), IOException, try)
import Control.Lens
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators (whenLeft, fromRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq
import qualified System.Directory as D
import System.FilePath ((</>), hasTrailingPathSeparator, joinPath, splitPath)
import System.IO.Temp (withSystemTempDirectory)

data SnapshotError
  = SnapshotDirectoryMissingE
  | SnapshotIncoherent String
  | SnapshotProcessError SnapshotProcessError

data SnapshotProcessError
  = FetchSnapshotArchive String
  | DirectorySetupSnapshotArchive IOException
  | ExtractSnapshotArchive IOException

explainSnapshotError :: SnapshotError -> String
explainSnapshotError =
  \case
    SnapshotDirectoryMissingE -> "Snapshot directory is missing"
    SnapshotIncoherent e -> "Snapshot directory is incoherent: " <> e
    SnapshotProcessError e ->
      unlines
        [ "An exception occurred during snapshot processing:",
          case e of
            FetchSnapshotArchive x -> "Fetching failed with: " <> show x
            DirectorySetupSnapshotArchive x -> "Directory setup got an exception: " <> show x
            ExtractSnapshotArchive x -> "Extraction got an exception: " <> displayException x
        ]

newtype SnapshotUrl = SnapshotUrl {getSnapshotUrl :: String}

data SnapshotRepositoryStatus
  = SnapshotDirectoryMissing
  | SnapshotDirectoryInitialized
  -- | Includes stray content from a pre-snapshot git clone (e.g. a stale
  -- @.git@), which must be healed by a full overwrite.
  | SnapshotDirectoryIncoherent
  deriving stock (Eq, Show)

snapshotRepositoryStatus :: FilePath -> IO SnapshotRepositoryStatus
snapshotRepositoryStatus root = do
  dirExists <- D.doesDirectoryExist root
  if dirExists
    then do
      dirAdvisoriesExists <- D.doesDirectoryExist $ root </> "advisories"
      etagMetadataExists <- D.doesFileExist $ root </> "snapshot-etag"
      staleGitExists <-
        (||)
          <$> D.doesFileExist (root </> ".git")
          <*> D.doesDirectoryExist (root </> ".git")
      return $
        if dirAdvisoriesExists && etagMetadataExists && not staleGitExists
          then SnapshotDirectoryInitialized
          else SnapshotDirectoryIncoherent
    else return SnapshotDirectoryMissing

data SnapshotRepositoryEnsuredStatus
  = SnapshotRepositoryCreated
  | SnapshotRepositoryExisting

ensureSnapshot ::
  FilePath ->
  SnapshotUrl ->
  SnapshotRepositoryStatus ->
  ExceptT SnapshotError IO SnapshotRepositoryEnsuredStatus
ensureSnapshot root url =
  \case
    SnapshotDirectoryMissing -> do
      overwriteSnapshot root url
      return SnapshotRepositoryCreated
    SnapshotDirectoryIncoherent -> do
      overwriteSnapshot root url
      return SnapshotRepositoryCreated
    SnapshotDirectoryInitialized ->
      return SnapshotRepositoryExisting

overwriteSnapshot :: FilePath -> SnapshotUrl -> ExceptT SnapshotError IO ()
overwriteSnapshot root url =
  withExceptT SnapshotProcessError $ do
    ensuringPerformed <- liftIO $ try $ ensureEmptyRoot root
    whenLeft ensuringPerformed $
      throwE . DirectorySetupSnapshotArchive

    resultE <- liftIO $ try $ get $ getSnapshotUrl url
    case resultE of
      Left e ->
        throwE $
          FetchSnapshotArchive $
            case e of
              InvalidUrlException url' reason ->
                "Invalid URL " <> show url' <> ": " <> show reason
              HttpExceptionRequest _ content ->
                case content of
                  StatusCodeException response body ->
                    "Request (GET " <> getSnapshotUrl url <> ") failed with " <> show (response ^. responseStatus) <> ": " <> show body
                  _ ->
                    "Request (GET " <> getSnapshotUrl url <> ") failed: " <> show content
      Right result -> do
        performed <-
          liftIO $
            try $
              withSystemTempDirectory "security-advisories" $ \tempDir -> do
                let archivePath = tempDir <> "/snapshot-export.tar.gz"
                BL.writeFile archivePath $ result ^. responseBody
                contents <- BL.readFile archivePath
                let fixEntry e = e { Tar.entryTarPath = fixEntryPath $ Tar.entryTarPath e }
                    fixEntryPath :: Tar.TarPath -> Tar.TarPath
                    fixEntryPath p =
                      fromRight p $
                        maybe
                          (Right p)
                          (Tar.toTarPath (hasTrailingPathSeparator $ Tar.fromTarPath p) . joinPath) $
                        stripRootPath $
                        splitPath $
                        Tar.fromTarPath p
                    stripRootPath =
                      \case
                        ("/":_:p:ps) -> Just (p:ps)
                        (_:p:ps) -> Just (p:ps)
                        [p] | hasTrailingPathSeparator p -> Nothing
                        ps -> Just ps
                Tar.unpack root $ Tar.mapEntriesNoFail fixEntry $ Tar.read $ GZip.decompress contents
        whenLeft performed $
          throwE . ExtractSnapshotArchive

        etagWritten <-
          liftIO $
            try $
              T.writeFile (root </> "snapshot-etag") $
                T.decodeUtf8 $
                  result ^. responseHeader "etag"
        whenLeft etagWritten $
          throwE . ExtractSnapshotArchive

-- | Delete the entire tree at @root@ (if it exists) and recreate it as an
-- empty directory. Destructive: only for use on snapshot-owned roots
-- (as done by 'overwriteSnapshot').
ensureEmptyRoot :: FilePath -> IO ()
ensureEmptyRoot root = do
  rootExists <- D.doesDirectoryExist root
  if rootExists
    then do
      -- If root itself is a symlink, remove only the link, never the target.
      isSymlink <- D.pathIsSymbolicLink root
      if isSymlink
        then D.removeDirectoryLink root
        else D.removeDirectoryRecursive root
    else pure ()
  D.createDirectoryIfMissing True root

newtype SnapshotDirectoryInfo = SnapshotDirectoryInfo
  { etag :: ETag
  }
  deriving stock (Eq, Show)

newtype ETag = ETag T.Text
  deriving stock (Eq, Show)

getDirectorySnapshotInfo :: FilePath -> IO (Either SnapshotError SnapshotDirectoryInfo)
getDirectorySnapshotInfo root =
  runExceptT $ do
    let metadataPath = root </> "snapshot-etag"
    unlessM (liftIO $ D.doesFileExist metadataPath) $
      throwE SnapshotDirectoryMissingE

    SnapshotDirectoryInfo . ETag <$> liftIO (T.readFile metadataPath)

latestUpdate :: SnapshotUrl -> ExceptT SnapshotError IO ETag
latestUpdate url =
  withExceptT SnapshotProcessError $ do
    resultE <- liftIO $ try $ headWith (defaults & redirects .~ 3) $ getSnapshotUrl url
    case resultE of
      Left e ->
        throwE $
          FetchSnapshotArchive $
            case e of
              InvalidUrlException url' reason ->
                "Invalid URL " <> show url' <> ": " <> show reason
              HttpExceptionRequest _ content ->
                case content of
                  StatusCodeException response body ->
                    "Request (HEAD " <> getSnapshotUrl url <> ") failed with " <> show (response ^. responseStatus) <> ": " <> show body
                  _ ->
                    "Request (HEAD " <> getSnapshotUrl url <> ") failed: " <> show content
      Right result ->
        case result ^? responseHeader "etag" of
          Nothing -> throwE $ FetchSnapshotArchive "Missing ETag header"
          Just rawETag -> return $ ETag $ T.decodeUtf8 rawETag
