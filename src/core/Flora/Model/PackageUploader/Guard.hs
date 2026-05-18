module Flora.Model.PackageUploader.Guard where

import Data.Text
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types

guardThatPackageUploaderExists
  :: (DB :> es, Trace :> es)
  => Text
  -> PackageIndexId
  -> Eff es PackageUploader
  -- ^ Action to run if the package does not exist
  -> Eff es PackageUploader
guardThatPackageUploaderExists username packageIndexId action =
  Tracing.childSpan "guardThatPackageUploaderExists" $ do
    result <-
      Tracing.childSpan "Query.getPackageUploaderByUsernameAndIndex" $
        Query.getPackageUploaderByUsernameAndIndex username packageIndexId
    case result of
      Nothing -> action
      Just packageUploader -> pure packageUploader
