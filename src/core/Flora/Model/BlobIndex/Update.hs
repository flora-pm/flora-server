module Flora.Model.BlobIndex.Update where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (LazyByteString)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text.Display (display)
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time (Time)
import Log qualified

import Database.PostgreSQL.Entity (Entity, _insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.Types (Query)
import Database.PostgreSQL.Transact (DBT)

import Distribution.Version (Version)

import Flora.Model.BlobIndex.Internal
import Flora.Model.BlobIndex.Types
import Flora.Model.BlobStore.API
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release (..), ReleaseId (..))
import Flora.Model.Release.Update qualified as Update

insertTar
  :: (Log :> es, DB :> es, BlobStoreAPI :> es, Time :> es)
  => PackageName
  -> Version
  -> LazyByteString
  -> Eff es (Either BlobStoreInsertError Sha256Sum)
insertTar pname version contents = do
  mpackage <- Query.getPackageByNamespaceAndName (Namespace "hackage") pname
  case mpackage of
    Nothing -> pure . Left $ NoPackage pname
    Just package -> do
      mrelease <- Query.getReleaseByVersion (package.packageId) version
      case mrelease of
        Nothing -> pure . Left $ NoRelease pname version
        Just release -> case hashTree <$> tarballToTree pname version contents of
          Left err -> pure . Left $ BlobStoreTarError pname version err
          Right t@(TarRoot rootHash _ _ _) -> Right rootHash <$ insertTree (release.releaseId) t

insertTree
  :: (Log :> es, DB :> es, BlobStoreAPI :> es)
  => ReleaseId
  -> TarRoot Sha256Sum
  -> Eff es ()
insertTree releaseId t@(TarRoot rootHash _ _ tree) = do
  Log.logTrace "Trying to insert directory tree" t
  mTarballHash <- Query.getReleaseTarballHash releaseId
  case mTarballHash of
    Just tarballHash -> Log.logInfo_ $ "Hash already inserted with hash: " <> display tarballHash
    Nothing -> do
      Update.updateTarballHash releaseId rootHash
      void $! M.traverseWithKey (insertBlobs rootHash) tree
      Log.logInfo_ $ "Inserted hash tree with root " <> display rootHash
  where
    _onConflictDoNothing :: Query
    _onConflictDoNothing = fromString "on conflict do nothing"

    insertDoNothing :: forall e m. (ToRow e, Entity e, MonadIO m) => e -> DBT m Int64
    insertDoNothing = execute Update (_insert @e <> _onConflictDoNothing)

    insertBlobs parentHash dir (TarDirectory childHash nodes) = do
      res <- dbtToEff . insertDoNothing $! BlobRelation parentHash childHash dir True
      when (res > 0) $! void $ M.traverseWithKey (insertBlobs childHash) nodes
      void . dbtToEff . insertDoNothing $! BlobRelation parentHash childHash dir True
    insertBlobs parentHash dir (TarFile childHash content) = do
      put childHash content
      void . dbtToEff . insertDoNothing $! BlobRelation parentHash childHash dir False
