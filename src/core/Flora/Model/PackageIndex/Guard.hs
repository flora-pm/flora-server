module Flora.Model.PackageIndex.Guard where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace

import Flora.Database
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Monad

guardThatPackageIndexExists
  :: (IOE :> es, Tracer :> es)
  => Pool Connection
  -> Text
  -> Eff es PackageIndex
  -- ^ Action to run if the package does not exist
  -> FloraM es PackageIndex
guardThatPackageIndexExists pool indexName action =
  Trace.withSpan "guardThatPackageIndexExists" $ do
    result <-
      Trace.withSpan "Query.getPackageIndexByName" $
        withReadOnlyPool pool $
          Query.getPackageIndexByName indexName
    case result of
      Nothing -> action
      Just packageIndex -> pure packageIndex
