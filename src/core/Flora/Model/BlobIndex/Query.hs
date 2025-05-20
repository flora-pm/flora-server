{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.BlobIndex.Query
  ( queryTar
  ) where

import Control.Exception (throw)
import Data.ByteString.Lazy (LazyByteString)
import Data.Map qualified as M
import Data.Text.Display (display)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_orderBy, _selectWhere)
import Database.PostgreSQL.Entity.DBT ( query)
import Database.PostgreSQL.Entity.Types (SortKeyword (..), field)
import Database.PostgreSQL.Simple (Only (..))
import Distribution.Version (Version)
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Log qualified

import Flora.Model.BlobIndex.Internal
import Flora.Model.BlobIndex.Types (BlobRelation (..), BlobStoreQueryError (..))
import Flora.Model.BlobStore.API (BlobStoreAPI, get)
import Flora.Model.Package (PackageName)

-- | Query a package name, version and hash and construct a uncompressed tarball
-- from the database
queryTar
  :: forall es
   . (BlobStoreAPI :> es, DB :> es, Log :> es)
  => PackageName
  -> Version
  -> Sha256Sum
  -> Eff es LazyByteString
queryTar pname version rootHash = do
  Log.logInfo_ $ "Querying for " <> display rootHash
  children <- traverse go =<< queryChildren rootHash
  let tree = TarRoot rootHash pname version . M.fromList $ V.toList children
  pure $ treeToTarball tree
  where
    queryChildren :: Sha256Sum -> Eff es (V.Vector BlobRelation)
    queryChildren hash =
      dbtToEff $!
        query
          ( _selectWhere @BlobRelation [[field| blob_hash |]]
              -- Ensures we consistently get back the directory structure
              -- This may not be the same as the hackage tarball!
              <> _orderBy ([field| blob_dep_path |], ASC)
          )
          (Only hash)
    go :: BlobRelation -> Eff es (FilePath, TarTree Sha256Sum)
    go (BlobRelation _blobHash blobDepHash blobDepPath blobDepDirectory)
      | blobDepDirectory =
          (blobDepPath,)
            . TarDirectory blobDepHash
            . M.fromList
            . V.toList
            <$> (traverse go =<< queryChildren blobDepHash)
    go (BlobRelation _blobHash blobDepHash blobDepPath _blobDepDirectory) = do
      mcontent <- get blobDepHash
      case mcontent of
        Nothing -> throw $ IncompleteDirectoryTree pname version
        Just bytes ->
          pure
            ( blobDepPath
            , TarFile blobDepHash bytes
            )
