module Flora.Model.PackageIndex.Guard where

import Data.Text (Text)
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Trace
import Effectful.Tracing qualified as Tracing

import Flora.Database
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types

guardThatPackageIndexExists
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Tracer :> es)
  => Text
  -> Eff es PackageIndex
  -- ^ Action to run if the package does not exist
  -> Eff es PackageIndex
guardThatPackageIndexExists indexName action =
  Tracing.withSpan "guardThatPackageIndexExists" $ do
    result <-
      Tracing.withSpan "Query.getPackageIndexByName" $
        Query.getPackageIndexByName indexName
    case result of
      Nothing -> action
      Just packageIndex -> pure packageIndex
