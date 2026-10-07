module Flora.Model.PackageIndex.Guard where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Effectful

import Flora.Database
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Monad

guardThatPackageIndexExists
  :: IOE :> es
  => Pool Connection
  -> Text
  -> Eff es PackageIndex
  -- ^ Action to run if the package does not exist
  -> FloraM es PackageIndex
guardThatPackageIndexExists pool indexName action = do
  result <-
    withReadOnlyPool pool $
      Query.getPackageIndexByName indexName
  case result of
    Nothing -> action
    Just packageIndex -> pure packageIndex
