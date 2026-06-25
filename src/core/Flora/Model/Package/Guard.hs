module Flora.Model.Package.Guard where

import Effectful
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace

import Flora.Database
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types

guardThatPackageExists
  :: (IOE :> es, ReadDB :> es, Tracer :> es)
  => Namespace
  -> PackageName
  -> (Namespace -> PackageName -> Eff es Package)
  -- ^ Action to run if the package does not exist
  -> Eff es Package
guardThatPackageExists namespace packageName action =
  Trace.withSpan "guardThatPackageExists " $ do
    result <-
      Trace.withSpan "Query.getPackageByNamespaceAndName " $
        Query.getPackageByNamespaceAndName namespace packageName
    case result of
      Nothing -> action namespace packageName
      Just package ->
        case package.status of
          FullyImportedPackage -> pure package
          UnknownPackage -> action namespace packageName
