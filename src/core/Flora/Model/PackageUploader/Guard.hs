module Flora.Model.PackageUploader.Guard where

import Data.Pool (Pool)
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import Effectful

import Flora.Database
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types
import Flora.Monad

guardThatPackageUploaderExists
  :: IOE :> es
  => Pool Connection
  -> Text
  -> PackageIndexId
  -> Eff es PackageUploader
  -- ^ Action to run if the package does not exist
  -> FloraM es PackageUploader
guardThatPackageUploaderExists pool username packageIndexId action = do
  result <-
    withReadOnlyPool pool $
      Query.getPackageUploaderByUsernameAndIndex username packageIndexId
  case result of
    Nothing -> action
    Just packageUploader -> pure packageUploader
