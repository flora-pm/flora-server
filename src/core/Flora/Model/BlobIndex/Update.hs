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
import Effectful.Labeled
import Effectful.Log (Log)
import Effectful.PostgreSQL
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Log qualified

import Flora.Database
import Flora.Environment.Env
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
  :: (BlobStoreAPI :> es, IOE :> es, Labeled ReadWrite WithConnection :> es, Log :> es, Reader FloraEnv :> es)
  => Namespace
  -> PackageName
  -> Version
  -> LazyByteString
  -> FloraM es (Either BlobStoreInsertError Sha256Sum)
insertTar namespace packageName version contents = do
  FloraEnv{pool} <- Reader.ask
  mpackage <- withReadOnlyPool pool $ Query.getPackageByNamespaceAndName namespace packageName
  case mpackage of
    Nothing -> pure . Left $ NoPackage packageName
    Just package -> do
      mrelease <- withReadOnlyPool pool $ Query.getReleaseByVersion package.packageId version
      case mrelease of
        Nothing -> pure . Left $ NoRelease packageName version
        Just release -> do
          Update.updateTarballArchiveHash release.releaseId contents
          case hashTree <$> tarballToTree packageName version contents of
            Left err -> pure . Left $ BlobStoreTarError packageName version err
            Right t@(TarRoot rootHash _ _ _) -> Right rootHash <$ insertTree release.releaseId t

insertTree
  :: (BlobStoreAPI :> es, IOE :> es, Labeled ReadWrite WithConnection :> es, Log :> es, Reader FloraEnv :> es)
  => ReleaseId
  -> TarRoot Sha256Sum
  -> FloraM es ()
insertTree releaseId t@(TarRoot rootHash _ _ tree) = do
  FloraEnv{pool} <- Reader.ask
  Log.logTrace "Trying to insert directory tree" t
  mTarballHash <- withReadOnlyPool pool $ Query.getReleaseTarballRootHash releaseId
  case mTarballHash of
    Just tarballHash -> Log.logInfo_ $ "Hash already inserted with hash: " <> display tarballHash
    Nothing -> do
      Update.updateTarballRootHash releaseId rootHash
      void $! M.traverseWithKey (insertBlobs rootHash) tree
      Log.logInfo_ $ "Inserted hash tree with root " <> display rootHash
  where
    _onConflictDoNothing :: Query
    _onConflictDoNothing = fromString "on conflict do nothing"

    insertDoNothing :: forall e es. (Entity e, IOE :> es, Labeled ReadWrite WithConnection :> es, ToRow e) => e -> Eff es Int64
    insertDoNothing params = labeled @ReadWrite @WithConnection $ execute (_insert @e <> _onConflictDoNothing) params

    insertBlobs parentHash dir (TarDirectory childHash nodes) = do
      res <- insertDoNothing $! BlobRelation parentHash childHash dir True
      when (res > 0) $! void $ M.traverseWithKey (insertBlobs childHash) nodes
      void . insertDoNothing $! BlobRelation parentHash childHash dir True
    insertBlobs parentHash dir (TarFile childHash content) = do
      put childHash content
      void . insertDoNothing $! BlobRelation parentHash childHash dir False
