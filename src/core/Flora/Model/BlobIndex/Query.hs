{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.BlobIndex.Query
  ( queryTar
  ) where

import Control.Exception (throw)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Text.Display
import Data.Vector qualified as V

import Database.PostgreSQL.Entity (_orderBy, _selectWhere)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Types (SortKeyword (..), field)
import Database.PostgreSQL.Simple (Only (..))
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Log qualified

import Distribution.Version (Version)
import Flora.Model.BlobIndex.Types
import Flora.Model.Package (PackageName)

import Flora.Model.BlobIndex.Internal
import Flora.Model.BlobStore.API

-- | Query a package name, version and hash and construct a uncompressed tarball
-- from the database
queryTar
  :: forall es
   . (Log :> es, DB :> es, BlobStoreAPI :> es)
  => PackageName
  -> Version
  -> Sha256Sum
  -> Eff es BL.ByteString
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
          Select
          ( _selectWhere @BlobRelation [[field| blob_hash |]]
              -- Ensures we consistently get back the directory structure
              -- This may not be the same as the hackage tarball!
              <> _orderBy ([field| blob_dep_path |], ASC)
          )
          (Only hash)
    go :: BlobRelation -> Eff es (FilePath, TarTree Sha256Sum)
    go BlobRelation{..}
      | blobDepDirectory =
          (blobDepPath,)
            . TarDirectory blobDepHash
            . M.fromList
            . V.toList
            <$> (traverse go =<< queryChildren blobDepHash)
    go BlobRelation{..} = do
      mcontent <- get blobDepHash
      case mcontent of
        Nothing -> throw $ IncompleteDirectoryTree pname version
        Just bytes ->
          pure
            ( blobDepPath
            , TarFile blobDepHash bytes
            )
