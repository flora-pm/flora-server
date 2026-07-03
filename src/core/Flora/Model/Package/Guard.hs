module Flora.Model.Package.Guard where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace

import Flora.Database
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Monad

guardThatPackageExists
  :: (IOE :> es, Tracer :> es)
  => Pool Connection
  -> Namespace
  -> PackageName
  -> FloraM es (Maybe Package)
guardThatPackageExists pool namespace packageName =
  Trace.withSpan "guardThatPackageExists " $ do
    result <-
      Trace.withSpan "Query.getPackageByNamespaceAndName " $
        withReadOnlyPool pool $
          Query.getPackageByNamespaceAndName namespace packageName
    pure $ case result of
      Just package ->
        case package.status of
          FullyImportedPackage -> Just package
          UnknownPackage -> Nothing
      Nothing -> Nothing
