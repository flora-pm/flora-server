module Flora.Model.PackageIndex.Guard where

import Data.Text (Text)
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Database
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types

guardThatPackageIndexExists
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Trace :> es)
  => Text
  -> Eff es PackageIndex
  -- ^ Action to run if the package does not exist
  -> Eff es PackageIndex
guardThatPackageIndexExists indexName action =
  Tracing.childSpan "guardThatPackageIndexExists" $ do
    result <-
      Tracing.childSpan "Query.getPackageIndexByName" $
        Query.getPackageIndexByName indexName
    case result of
      Nothing -> action
      Just packageIndex -> pure packageIndex
