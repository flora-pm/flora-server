module Flora.Model.BlobIndex.Update where

import Control.Monad (void, when)
import Data.ByteString.Lazy (LazyByteString)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text.Display (display)
import Database.PostgreSQL.Entity (Entity, _insert)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.Types (Query)
import Distribution.Version (Version)
import Effectful
import Effectful.Log (Log)
import Log qualified

import Flora.Database
import Flora.Model.BlobIndex.Internal
import Flora.Model.BlobIndex.Types
import Flora.Model.BlobStore.API
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release (..), ReleaseId (..))
import Flora.Model.Release.Update qualified as Update
import Flora.Monad

insertTar
  :: (BlobStoreAPI :> es, IOE :> es, Log :> es, ReadDB :> es, WriteDB :> es)
  => Namespace
  -> PackageName
  -> Version
  -> LazyByteString
  -> FloraM es (Either BlobStoreInsertError Sha256Sum)
insertTar namespace packageName version contents = do
  lookups <- do
    mpackage <- Query.getPackageByNamespaceAndName namespace packageName
    case mpackage of
      Nothing -> pure . Left $ NoPackage packageName
      Just package -> do
        mrelease <- Query.getReleaseByVersion package.packageId version
        case mrelease of
          Nothing -> pure . Left $ NoRelease packageName version
          Just release -> do
            existing <- Query.getReleaseTarballRootHash release.releaseId
            pure $ Right (release, existing)
  case lookups of
    Left err -> pure $ Left err
    Right (_release, Just rootHash) -> do
      Log.logInfo_ $ "Tarball already inserted with root " <> display rootHash
      pure $ Right rootHash
    Right (release, Nothing) -> do
      Update.updateTarballArchiveHash release.releaseId contents
      case hashTree <$> tarballToTree packageName version contents of
        Left err -> pure . Left $ BlobStoreTarError packageName version err
        Right t@(TarRoot rootHash _ _ _) -> Right rootHash <$ insertTree release.releaseId t

insertTree
  :: (BlobStoreAPI :> es, IOE :> es, Log :> es, WriteDB :> es)
  => ReleaseId
  -> TarRoot Sha256Sum
  -> FloraM es ()
insertTree releaseId (TarRoot rootHash _ _ tree) = do
  Log.logInfo_ $
    "Inserting tarball tree with root " <> display rootHash <> " (" <> display (M.size tree) <> " top-level nodes)"
  Update.updateTarballRootHash releaseId rootHash
  void $! M.traverseWithKey (insertBlobs rootHash) tree
  Log.logInfo_ $ "Inserted hash tree with root " <> display rootHash
  where
    _onConflictDoNothing :: Query
    _onConflictDoNothing = fromString "on conflict do nothing"

    insertDoNothing :: forall e es. (Entity e, IOE :> es, ToRow e, WriteDB :> es) => e -> Eff es Int64
    insertDoNothing params = execute (_insert @e <> _onConflictDoNothing) params

    insertBlobs parentHash dir (TarDirectory childHash nodes) = do
      res <- insertDoNothing $! BlobRelation parentHash childHash dir True
      when (res > 0) $! void $ M.traverseWithKey (insertBlobs childHash) nodes
    insertBlobs parentHash dir (TarFile childHash content) = do
      put childHash content
      void . insertDoNothing $! BlobRelation parentHash childHash dir False
