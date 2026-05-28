module Flora.Model.PackageUploader.Guard where

import Data.Text
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Trace
import Effectful.Tracing qualified as Tracing

import Flora.Database
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types

guardThatPackageUploaderExists
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Tracer :> es)
  => Text
  -> PackageIndexId
  -> Eff es PackageUploader
  -- ^ Action to run if the package does not exist
  -> Eff es PackageUploader
guardThatPackageUploaderExists username packageIndexId action =
  Tracing.withSpan "guardThatPackageUploaderExists" $ do
    result <-
      Tracing.withSpan "Query.getPackageUploaderByUsernameAndIndex" $
        Query.getPackageUploaderByUsernameAndIndex username packageIndexId
    case result of
      Nothing -> action
      Just packageUploader -> pure packageUploader
