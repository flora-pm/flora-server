module Flora.Model.PackageIndex.Guard where

import Data.Text (Text)
import Effectful
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace

import Flora.Database
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types

guardThatPackageIndexExists
  :: (IOE :> es, ReadDB :> es, Tracer :> es)
  => Text
  -> Eff es PackageIndex
  -- ^ Action to run if the package does not exist
  -> Eff es PackageIndex
guardThatPackageIndexExists indexName action =
  Trace.withSpan "guardThatPackageIndexExists" $ do
    result <-
      Trace.withSpan "Query.getPackageIndexByName" $
        Query.getPackageIndexByName indexName
    case result of
      Nothing -> action
      Just packageIndex -> pure packageIndex
